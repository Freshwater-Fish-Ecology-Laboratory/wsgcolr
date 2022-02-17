event_flag <- function(duration, max_absence, lag_receiver_group, receiver_group, flag, path){
  if(path){
    x <- duration > max_absence
  } else {
    x <- duration > max_absence | lag_receiver_group != receiver_group
  }
  x[1] <- TRUE
  if(flag)
    return(x)
  cumsum(x)
}

seq_event <- function(start, end, tstep){
  dtflr <- lubridate::floor_date(start, unit = tstep)
  seq(dtflr, end, tstep)
}

convert_tstep <- function(tstep){
  chk_subset(tstep, c("month", "week", "day", "year"))
  n <- 1
  units <- paste0(tstep, "s")
  
  if(tstep == "week"){
    n <- 7
    units <- "days"
  }
  list(n = n,
       units = units)
}

tstep_interval <- function(start, end, tstep){
  x <- seq_event(start, end, tstep)
  units <- convert_tstep(tstep)
  lubridate::interval(x, dttr2::dtt_add_units(x, units = units$units, n = units$n))
}

#' Calculate duration of a timestep
#' 
#' @inheritParams params
#' @return An integer
#'
#' @export
duration_tstep <- function(tstep){
  units <- convert_tstep(tstep)
  t <- as.Date("2010-01-01")
  interv <- lubridate::interval(t, dttr2::dtt_add_units(t, units = units$units, n = units$n))
  lubridate::time_length(interv)
}
  
duration_interval <- function(x1, x2){
  lubridate::time_length(lubridate::intersect(x1, x2))
}

get_residence_event <- function(detection, 
                                max_absence = 96,
                                min_detections = 2,
                                min_duration = 6,
                                path = FALSE,
                                receiver_group = "receiver_group", 
                                datetime = "datetime_pst"){
  
  # chk_detection_(detection_timestep)
  chk_string(receiver_group)
  chk_string(datetime)
  chk_number(max_absence)
  chk_number(min_duration)
  chk_number(min_detections)
  chk_flag(path)
  
  max_absence_s <- max_absence*3600
  min_duration_s <- min_duration*3600
  
  x <- detection %>%
    ## determine new events
    group_by(.data$transmitter) %>%
    arrange(.data$transmitter, !! sym(datetime), !! sym(receiver_group)) %>%
    mutate(duration = as.numeric(difftime(!! sym(datetime), 
                                          lag(!! sym(datetime)), units = "secs")),
           duration = if_else(is.na(.data$duration), 0, duration),
           new_event = event_flag(duration = .data$duration, 
                                  max_absence = max_absence_s, 
                                  lag_receiver_group = lag(!! sym(receiver_group)), 
                                  receiver_group = !! sym(receiver_group),
                                  path = path,
                                  flag = TRUE),
           event = event_flag(duration = .data$duration, 
                              max_absence = max_absence_s, 
                              lag_receiver_group = lag(!! sym(receiver_group)), 
                              receiver_group = !! sym(receiver_group),
                              path = path,
                              flag = FALSE)) %>%
    ungroup() %>%
  
    ### filter if more than min_detections per event
    group_by(.data$transmitter, .data$event) %>%
    mutate(ndetections = n(),
           duration_s = as.numeric(difftime(last(!! sym(datetime)), 
                                            first(!! sym(datetime))), units = "secs")) %>%
    ungroup() %>%
    filter(ndetections >= min_detections) %>%
    filter(duration_s >= min_duration_s) %>%
    ### recreate event column accouting for those removed in above step
    group_by(.data$transmitter) %>%
    arrange(.data$transmitter, !! sym(datetime), !! sym(receiver_group)) %>%
    mutate(event = cumsum(new_event)) %>%
    ungroup() %>%
    arrange(.data$transmitter, .data$event) 
  
  x
}

#' Create detection residence events
#'
#' A new residence event occurs if a fish moves to a new receiver group, more than
#'  `max_absence` time passes without a detection, more than `min_detections` occurs
#'  and more than `min_duration` occurs.
#'
#' @inheritParams params
#' @return A tibble.
#'
#' @export
residence_event <- function(detection, 
                            max_absence = 96,
                            min_detections = 2,
                            min_duration = 6,
                            squash = TRUE,
                            receiver_group = "receiver_group", 
                            datetime = "datetime_pst"){
  
  chk_flag(squash)
  
  x <- get_residence_event(detection = detection,
                           max_absence = max_absence,
                           min_detections = min_detections,
                           min_duration = min_duration,
                           path = FALSE,
                           receiver_group = receiver_group,
                           datetime = datetime)
  
  ### return data with added event column
  if(!squash)
    return(x %>% select(-ndetections, -duration_s, -duration, -new_event))
  
  ### get mean rkm weighted by ndetections at stations
  location <- x %>%
    group_by(.data$transmitter, .data$event, .data$station_id, 
             .data$rkm) %>%
    summarize(ndets = n()) %>%
    ungroup() %>%
    group_by(.data$transmitter, .data$event) %>%
    summarize(mean_rkm = weighted.mean(rkm, ndets)) %>%
    ungroup()
  
  ### get event start/end and join to location by transmitter/event
  x <- x %>%
    group_by(transmitter, !! sym(receiver_group), event, ndetections, duration_s) %>%
    arrange(!! sym(datetime)) %>%
    summarize(event_start = first(!! sym(datetime)),
              event_end = last(!! sym(datetime))) %>%
    ungroup() %>%
    arrange(transmitter, event) %>%
    left_join(location, c("transmitter", "event"))
  
  x
}

#' Create detection residence paths
#'
#' A new residence path occurs if more than `max_absence` time passes without
#'  a detection, more than `min_detections` occurs and more than `min_duration` occurs.
#'  Movement between receivers is ignored.
#'  
#' @inheritParams params
#' @return A tibble.
#'
#' @export
residence_path <- function(detection, 
                           max_absence = 96,
                           min_detections = 2,
                           min_duration = 6,
                           receiver_group = "receiver_group", 
                           datetime = "datetime_pst"){
  
  x <- get_residence_event(detection = detection,
                           max_absence = max_absence,
                           min_detections = min_detections,
                           min_duration = min_duration,
                           path = TRUE,
                           receiver_group = receiver_group,
                           datetime = datetime)
  
  x %>% select(-ndetections, -duration_s, -duration, -new_event)
  
}



#' Create abundance within timestep
#' 
#' Input should be output of `residence_event()` function with `squash = TRUE`. 
#' Abundance is the total number of unque transmitters calculated from 
#' `residence_proportion_complete()` grouped by receiver_group and timestep
#' 
#' @inheritParams params
#' @return A tibble.
#'
#' @export
abundance <- function(x, timestep = "week"){
  
  res <- residence_proportion(x, timestep = timestep) %>%
    residence_complete(timestep = timestep)
  
  abun <- res %>%
    group_by(receiver_group, timestep_start) %>%
    summarize(n = length(unique(transmitter)),
              abundance = sum(residence_presence),
              abundance_proportion = abundance/n) %>%
    ungroup()
  
  abun
  
}
