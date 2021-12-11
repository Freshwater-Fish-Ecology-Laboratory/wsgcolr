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
    group_by(station_id, logger_id) %>%
    dbplyr::window_order(date_deployment) %>%
    summarize(date_period_start = min(date_deployment, na.rm = TRUE),
              date_period_end = max(date_deployment, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(station_id, date_period_start)
  
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
    filter(datetime_pst >= date_period_start & datetime_pst <= date_period_end) %>%
    select(station_id, logger_id, datetime_pst, temperature_c)
  
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
    left_join(select(station, -insertion), "station_id") %>%
    left_join(select(receiver_group,  -max_rkm), "receiver_group") 
  
  if(clean){
    x <- x %>%
      select(station_id, logger_id, datetime_pst, temperature_c, station_name,  
             receiver_group, receiver_group_temp_zone, rkm, lon, lat, geom)
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