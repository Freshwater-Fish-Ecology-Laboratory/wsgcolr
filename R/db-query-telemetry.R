#' Query deployment periods using dbplyr SQL
#'
#' Get deployment start/end periods while taking into account the possibility of dead battery.
#' If battery is dead, gets last known detection and create new deployment period
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_deployment_period <- function(con, collect = TRUE){
  
  deployment <- db_read(con, "telemetry.deployment", collect = FALSE)
  detection <- db_read(con, "telemetry.detection", collect = FALSE)
  receiver <- db_read(con, "telemetry.receiver", collect = FALSE)
  
  x <- deployment %>%
    left_join(receiver, c("receiver" = "receiver_number")) %>%
    filter(.data$battery_dead) %>%
    ### get receiver id
    ### get last detection
    left_join(select(detection, .data$receiver, .data$datetime_utc), by = c("receiver_id" = "receiver")) %>%
    mutate(datetime_pst = timezone("Etc/GMT+8", .data$datetime_utc)) %>%
    filter(as.Date(.data$datetime_pst) <= .data$date_deployment) %>%
    group_by(.data$id) %>%
    arrange(.data$datetime_pst) %>%
    summarise(last_detection = as.Date(max(.data$datetime_pst, na.rm = TRUE))) %>%
    ungroup() %>%
    right_join(deployment, "id") %>%
    filter(.data$activity %in% c("deploy", "unknown") | (.data$download | is.na(.data$download))) %>%
    group_by(.data$station_id, .data$receiver) %>%
    dbplyr::window_order(.data$date_deployment) %>%
    mutate(date_period_start = .data$date_deployment,
           # if battery dead at next deployment and last detection at next deployment is later than date_deployment, then deployment_period_end is last_detection of next deployment
           date_period_end = if_else(lead(.data$battery_dead) & lead(.data$last_detection) > .data$date_deployment, lead(.data$last_detection), lead(.data$date_deployment))) %>%
    ungroup() %>%
    group_by(.data$station_id, .data$receiver) %>%
    dbplyr::window_order(.data$date_deployment) %>%
    # create a grouping variable that will indicate a new 'period' if the 
    # previous deployment end period does not equal period start (i.e. not a continuous deployment)
    mutate(cond = if_else(!is.na(lag(.data$date_period_end)) & .data$date_period_start != lag(.data$date_period_end), 1, 0)) %>%
    mutate(group = cumsum(.data$cond)) %>%
    ungroup() %>%
    # remove open-ended deployments
    filter(!is.na(.data$date_period_end)) %>%
    # now group by grouping variable created above and get min/max dates for period
    group_by(.data$station_id, .data$receiver, .data$group) %>%
    dbplyr::window_order(.data$date_deployment) %>%
    summarize(date_period_start = min(.data$date_period_start, na.rm = TRUE),
              date_period_end = max(.data$date_period_end, na.rm = TRUE), 
              .groups = c("keep")) %>%
    ungroup() %>%
    # get receiver_id not receiver_number
    left_join(receiver, c("receiver" = "receiver_number")) %>%
    select(.data$station_id, .data$receiver_id, .data$date_period_start, .data$date_period_end) %>%
    arrange(.data$station_id, .data$date_period_start) 
  
  if(collect)
    return(collect(x))
  x
}

#' Query clean detections using dbplyr SQL
#'
#' Converts timezone to PST and filters within deployment periods.
#' 
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_detection_clean <- function(con, collect = TRUE){
  detection_true <- db_read(con, "telemetry.detection_true", collect = FALSE)
  deployment_period <- db_query_deployment_period(con, collect = FALSE)
  x <- detection_true %>%
    select(.data$datetime_utc, .data$receiver, .data$transmitter) %>%
    mutate(datetime_pst = timezone("Etc/GMT+8", .data$datetime_utc)) %>%
    ### filter to deployment periods to get station
    left_join(deployment_period, by = c("receiver" = "receiver_id")) %>%
    filter(.data$datetime_pst >= .data$date_period_start & .data$datetime_pst <= .data$date_period_end) %>%
    select(.data$transmitter, .data$receiver, .data$station_id, .data$datetime_pst)
  
  if(collect)
    return(collect(x))
  x
}

#' Query clean detections and summarize by station and timestep
#' 
#' This is useful for visualizing deployments as it loses information about tag.
#'
#' @inheritParams params
#' @return A tibble of queried data.
#'
#' @export
db_query_detection_station <- function(con, timestep = "week", collect = TRUE){
  detection <- db_query_detection_clean(con, collect = FALSE)
  station <- db_read(con, "telemetry.station", collect = FALSE)
  x <- detection %>%
    mutate(timestep = as.Date(sql(glue::glue("date_trunc('{timestep}', datetime_pst)")))) %>%
    distinct(.data$timestep, .data$station_id) %>%
    left_join(station, "station_id")
  
  if(collect)
    return(collect(x))
  x
}

#' Query tidy captures using dbplyr SQL
#'
#' Joins capture table to transmitter and fish.
#' 
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_capture_tidy <- function(con, collect = TRUE){
  capture <- db_read(con, "telemetry.capture", collect = FALSE)
  fish <- db_read(con, "capture.fish", collect = FALSE)
  transmitter <- db_read(con, "telemetry.transmitter", collect = FALSE)
  
  x <- capture %>%
    left_join(select(fish, -.data$insertion), "pittag_id") %>%
    left_join(select(transmitter, -.data$insertion), "transmitter_id")
  
  if(collect)
    return(collect(x))
  x
}

#' Query tidy detections using dbplyr SQL
#'
#' Joins clean detections to tidy captures, station and receiver_group.
#' 
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_detection_tidy <- function(con, clean = TRUE, collect = TRUE){
  detection_clean <- db_query_detection_clean(con, collect = FALSE)
  capture_tidy <- db_query_capture_tidy(con, collect = FALSE)
  station <- db_read(con, "telemetry.station", collect = FALSE)
  receiver_group <- db_read(con, "telemetry.receiver_group", collect = FALSE)
  
  x <- detection_clean %>%
    left_join(capture_tidy, by = c("transmitter" = "transmitter_id")) %>%
    left_join(select(station, -.data$insertion), "station_id") %>%
    left_join(select(receiver_group,  -.data$max_rkm), "receiver_group")
  
  if(clean){
    x <- x %>%
      select(.data$transmitter, .data$receiver_group, .data$datetime_pst,
             .data$pittag_id, .data$tag_insertion_date,
             .data$forklength_cm, .data$weight_kg, .data$sex, .data$receiver_group_rkm, 
             .data$receiver_group_temp_zone, .data$receiver_group_flow_zone)
    if(collect){
      return(x %>% 
               collect() %>%
               mutate(receiver_group = forcats::fct_rev(forcats::fct_reorder(.data$receiver_group, .data$receiver_group_rkm)),
                      station_id = forcats::fct_rev(forcats::fct_reorder(.data$station_id, .data$rkm)),
                      station_name = forcats::fct_rev(forcats::fct_reorder(.data$station_name, .data$rkm))))
    }
  }
  
  if(collect)
    return(collect(x))
  x
}

#' Query tidy detections and summarize by timestep using dbplyr SQL
#'
#' Grouping is done by receiver_group. Receiver group with the most detections during timestep 'wins'.
#' 
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_detection_timestep <- function(con, timestep = "week", clean = TRUE, collect = TRUE){
  detection_tidy <- db_query_detection_tidy(con, collect = FALSE)
  capture_tidy <- db_query_capture_tidy(con, collect = FALSE)
  receiver_group <- db_read(con, "telemetry.receiver_group", collect = FALSE)
  
  x <- detection_tidy %>%
    mutate(timestep = sql(glue::glue("date_trunc('{timestep}', datetime_pst)"))) %>%
    group_by(.data$transmitter, .data$timestep, .data$receiver_group) %>%
    summarize(ndetects = n()) %>%
    ungroup() %>%
    group_by(.data$transmitter, .data$timestep) %>%
    slice_max(.data$ndetects) %>%
    ungroup() %>%
    left_join(capture_tidy, by = c("transmitter" = "transmitter_id")) %>%
    left_join(select(receiver_group,  -.data$max_rkm), "receiver_group")
  
  ### if collect + clean also adds nice factor to receiver group
  if(clean){
    x <- x %>%
      select(.data$transmitter, .data$receiver_group, .data$timestep, .data$ndetects, 
             .data$pittag_id, .data$tag_insertion_date,
             .data$forklength_cm, .data$weight_kg, .data$sex, .data$receiver_group_rkm, 
             .data$receiver_group_temp_zone, .data$receiver_group_flow_zone)
    if(collect){
      return(x %>% 
               collect() %>%
               mutate(receiver_group = forcats::fct_rev(forcats::fct_reorder(.data$receiver_group, .data$receiver_group_rkm))))
    }
  }
     
  if(collect)
    return(collect(x))
  x
}



