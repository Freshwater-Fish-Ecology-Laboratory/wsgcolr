#' Query temperature deployment periods using dbplyr SQL
#'
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_temperature_deployment_period <- function(con, collect = TRUE){
  temp_deployment <- db_read(con, "environmental.temperature_deployment", collect = FALSE)
  x <- temp_deployment %>%
    # filter(download | is.na(download)) %>%
    group_by(.data$station_id, .data$logger_id) %>%
    dbplyr::window_order(.data$date_deployment) %>%
    summarize(date_period_start = min(.data$date_deployment, na.rm = TRUE),
              date_period_end = max(.data$date_deployment, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(.data$station_id, .data$date_period_start)
  
  if(collect)
    return(collect(x))
  x
}

#' Query clean temperature data using dbplyr SQL
#' Clean data is filtered within deployments
#'
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_temperature_clean <- function(con, collect = TRUE){
  temp_deployment_period <- db_query_temperature_deployment_period(con, collect = FALSE)
  temp <- db_read(con, "environmental.temperature", collect = FALSE)
  
  x <- temp %>%
    left_join(temp_deployment_period, "logger_id") %>%
    filter(.data$datetime_pst >= .data$date_period_start & .data$datetime_pst <= .data$date_period_end) %>%
    select(.data$station_id, .data$logger_id, .data$datetime_pst, .data$temperature_c)
  
  if(collect)
    return(collect(x))
  x
}

#' Query tidy temperature data using dbplyr SQL
#' Tidy data includes joins to station and receiver group
#'
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_temperature_tidy <- function(con, clean = TRUE, collect = TRUE){
  temperature_clean <- db_query_temperature_clean(con, collect = FALSE)
  station <- db_read(con, "telemetry.station", collect = FALSE)
  receiver_group <- db_read(con, "telemetry.receiver_group", collect = FALSE)
  
  x <- temperature_clean %>%
    left_join(select(station, -.data$insertion), "station_id") %>%
    left_join(select(receiver_group,  -.data$max_rkm), "receiver_group") %>%
    dbplyr::window_order(station_id, datetime_pst)
  
  if(clean){
    x <- x %>%
      select(.data$station_id, .data$logger_id, .data$datetime_pst, .data$temperature_c,
             .data$station_name, .data$receiver_group, .data$receiver_group_temp_zone, 
             .data$rkm, .data$lon, .data$lat, .data$geom)
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

#' Query tidy discharge data using dbplyr SQL
#' Tidy data includes joins to discharge_station to get flow_zone
#'
#' @inheritParams params
#' @return A tibble of the queried data.
#'
#' @export
db_query_discharge_tidy <- function(con, clean = TRUE, collect = TRUE){
  discharge <- db_read(con, "environmental.discharge", collect = FALSE)
  discharge_station <- db_read(con, "environmental.discharge_station", sf = FALSE, collect = FALSE)
  
  x <- discharge %>%
    left_join(discharge_station, "station_id") %>%
    dbplyr::window_order(station_id, datetime_pst)
  
  if(clean){
    x <- x %>%
      select(-.data$lon, -.data$lat, -.data$geom)
    if(collect){
      return(x %>% 
               collect() %>%
               mutate(station_id = forcats::fct_rev(forcats::fct_reorder(.data$station_id, .data$flow_zone))))
    }
  }

  if(collect)
    return(collect(x))
  x
}