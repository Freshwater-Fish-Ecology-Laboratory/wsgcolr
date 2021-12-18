#' Calculate detection efficiency
#'
#' Calculates detection efficiency based on ping interval, timestep and deployments.
#'
#' @inheritParams params
#' @return A tibble.
#'
#' @export
calculate_de <- function(detection, deployment, ping_interval_s = 600, tstep = "day"){
  expected_pings <- duration_tstep(tstep)/ping_interval_s
  de <- dtplyr::lazy_dt(detection) %>%
    mutate(timestep = lubridate::floor_date(datetime_pst, unit = tstep)) %>%
    group_by(landmark_station, timestep, station_id, transmitter) %>%
    arrange(datetime_pst) %>%
    ### if detection within 3 seconds of each other only take one as from same pulse
    mutate(leadtime = difftime(lead(datetime_pst), datetime_pst, unit = "secs")) %>%
    filter(leadtime > 3 | is.na(leadtime)) %>%
    summarize(detects = n()) %>%
    ungroup() %>%
    dplyr::as_tibble() %>%
    group_by(landmark_station, station_id, transmitter) %>%
    # complete sequence of dates (including 0 detects)
    complete(timestep = seq.POSIXt(min(timestep), max(timestep), by = tstep), 
             fill = list(detects = 0)) %>%
    mutate(pings = expected_pings) %>% 
    ungroup()
  
  tmp <- de %>%
    filter(detects > pings)
  if(nrow(tmp) > 0){
    warning(glue::glue("There are {nrow(tmp)} cases where detects is greater than expected pings"))
    print(tmp)
  }
  
  ### change max detects to expected_pings
  de <- de %>%
    mutate(detects = if_else(detects > pings, pings, detects),
           de = detects/pings) 
  
  ## filter absences within deployments  
  # remove first and last day of deployment in case of partial days
  de <- de %>%
    left_join(deployment, c("station_id")) %>%
    filter(timestep >= date_period_start + days(1) & timestep <= date_period_end - days(1)) 
  
  de
}