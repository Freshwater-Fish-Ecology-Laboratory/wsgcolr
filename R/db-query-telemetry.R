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
    filter(battery_dead) %>%
    ### get receiver id
    ### get last detection
    left_join(select(detection, receiver, datetime_utc), by = c("receiver_id" = "receiver")) %>%
    filter(as.Date(datetime_utc) <= date_deployment) %>%
    group_by(id) %>%
    arrange(datetime_utc) %>%
    summarise(last_detection = as.Date(max(datetime_utc, na.rm = TRUE))) %>%
    ungroup() %>%
    right_join(deployment, "id") %>%
    filter(activity %in% c("deploy", "unknown") | (download | is.na(download))) %>%
    group_by(station_id, receiver) %>%
    dbplyr::window_order(date_deployment) %>%
    mutate(date_period_start = date_deployment,
           # if battery dead at next deployment and last detection at next deployment is later than date_deployment, then deployment_period_end is last_detection of next deployment
           date_period_end = if_else(lead(battery_dead) & lead(last_detection) > date_deployment, lead(last_detection), lead(date_deployment))) %>%
    ungroup() %>%
    group_by(station_id, receiver) %>%
    dbplyr::window_order(date_deployment) %>%
    # create a grouping variable that will indicate a new 'period' if the 
    # previous deployment end period does not equal period start (i.e. not a continuous deployment)
    mutate(cond = if_else(!is.na(lag(date_period_end)) & date_period_start != lag(date_period_end), 1, 0)) %>%
    mutate(group = cumsum(cond)) %>%
    ungroup() %>%
    # remove open-ended deployments
    filter(!is.na(date_period_end)) %>%
    # now group by grouping variable created above and get min/max dates for period
    group_by(station_id, receiver, group) %>%
    dbplyr::window_order(date_deployment) %>%
    summarize(date_period_start = min(date_period_start, na.rm = TRUE),
              date_period_end = max(date_period_end, na.rm = TRUE), 
              .groups = c("keep")) %>%
    ungroup() %>%
    # get receiver_id not receiver_number
    left_join(receiver, c("receiver" = "receiver_number")) %>%
    select(station_id, receiver_id, date_period_start, date_period_end) %>%
    arrange(station_id, date_period_start) 
  
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
    select(datetime_utc, receiver, transmitter) %>%
    mutate(datetime_pst = timezone("Etc/GMT+8", datetime_utc)) %>%
    ### filter to deployment periods to get station
    left_join(deployment_period, by = c("receiver" = "receiver_id")) %>%
    filter(datetime_pst >= date_period_start & datetime_pst <= date_period_end) %>%
    select(transmitter, receiver, station_id, datetime_pst)
  
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
    distinct(timestep, station_id) %>%
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
    left_join(select(fish, -insertion), "pittag_id") %>%
    left_join(select(transmitter, -insertion), "transmitter_id")
  
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
    left_join(select(station, -insertion), "station_id") %>%
    left_join(select(receiver_group,  -max_rkm), "receiver_group")
  
  if(clean){
    x <- x %>%
      select(transmitter, receiver_group, pittag_id, tag_insertion_date,
             forklength_cm, weight_kg, sex, receiver_group_rkm, 
             receiver_group_temp_zone, receiver_group_flow_zone)
    if(collect){
      return(x %>% 
               collect() %>%
               mutate(receiver_group = forcats::fct_rev(forcats::fct_reorder(receiver_group, receiver_group_rkm)),
                      station_id = forcats::fct_rev(forcats::fct_reorder(station_id, rkm)),
                      station_name = forcats::fct_rev(forcats::fct_reorder(station_name, rkm))))
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
    group_by(transmitter, timestep, receiver_group) %>%
    summarize(ndetects = n()) %>%
    ungroup() %>%
    group_by(transmitter, timestep) %>%
    slice_max(ndetects) %>%
    ungroup() %>%
    left_join(capture_tidy, by = c("transmitter" = "transmitter_id")) %>%
    left_join(select(receiver_group,  -max_rkm), "receiver_group")
  
  ### if collect + clean also adds nice factor to receiver group
  if(clean){
    x <- x %>%
      select(transmitter, receiver_group, timestep, ndetects, pittag_id, tag_insertion_date,
             forklength_cm, weight_kg, sex, receiver_group_rkm, 
             receiver_group_temp_zone, receiver_group_flow_zone)
    if(collect){
      return(x %>% 
               collect() %>%
               mutate(receiver_group = forcats::fct_rev(forcats::fct_reorder(receiver_group, receiver_group_rkm))))
    }
  }
     
  if(collect)
    return(collect(x))
  x
}



