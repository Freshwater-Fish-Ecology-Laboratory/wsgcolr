#' Aggregate detection by timestep
#'
#' Receiver group (location_column) per transmitter/timestep assigned by greatest number of detections.
#'
#' @inheritParams params
#' @return A modified tibble of the detection data.
#'
#' @export
detection_timestep <- function(detection, timestep = "week", receiver_group = "receiver_group"){

  chk_detection(detection)
  chk_timestep(timestep)
  chk_string(receiver_group)

  detection %>%
    mutate(timestep = as.Date(lubridate::floor_date(datetime_utc, unit = timestep))) %>%
    group_by(transmitter, timestep, !! sym(receiver_group)) %>%
    summarise(ndetection = n()) %>%
    ungroup() %>%
    group_by(transmitter, timestep) %>%
    slice_max(ndetection) %>%
    ungroup() 
}

#' Assign detections to events and paths
#'
#' A new detection event occurs if fish moves to new receiver group or if more than `max_absence` time passes without a detection.
#' A new detection path occurs if more than `max_absence` time passes without a detection (i.e., regardless of change in receiver group)
#'
#' @inheritParams params
#' @return A modified tibble of the detection events and paths.
#'
#' @export
detection_event <- function(detection_timestep, receiver_group = "receiver_group",
                            max_absence = 96, squash = FALSE){

  chk_detection_timestep(detection_timestep)
  chk_string(receiver_group)
  chk_whole_number(max_absence)
  chk_flag(squash)

  x <- detection_timestep %>%
    group_by(transmitter) %>%
    arrange(transmitter, timestep, !! sym(receiver_group)) %>%
    mutate(duration = as.numeric(difftime(timestep, lag(timestep), units = c("hours"))),
           duration = if_else(is.na(duration), 0, duration),
           new_event = duration > max_absence | lag(!! sym(receiver_group)) != !! sym(receiver_group),
           new_event = if_else(is.na(new_event), TRUE, new_event),
           new_path =  duration > max_absence,
           new_path = if_else(is.na(new_path), TRUE, new_path)) %>%
    mutate(event = cumsum(new_event),
           path = cumsum(new_path) + 1) %>%
    ungroup() %>%
    arrange(transmitter, event) %>%
    select(-new_path, -new_event, -duration)
  if(squash){
    x <- x %>%
      group_by(transmitter, !! sym(receiver_group), event) %>%
      arrange(timestep) %>%
      slice(c(1, n())) %>%
      ungroup()
  }
  x
}

#' Complete detection data
#'
#' Create detection presence/absence for each transmitter/timestep/receiver group combination
#' `timestep` should match what was used in `detection_timestep`.
#'
#' @inheritParams params
#' @return A modified tibble of the complete detection data.
#'
#' @export
detection_complete <- function(detection_timestep, timestep = "week", receiver_group = "receiver_group"){

  chk_detection_timestep(detection_timestep)
  chk_timestep(timestep)
  chk_string(receiver_group)

  timestep_range <- seq.Date(min(detection_timestep$timestep), max(detection_timestep$timestep), by = timestep)
  detection_timestep$present <- 1
  detection_timestep %>%
    arrange(transmitter_id, timestep, array) %>%
    tidyr::complete(timestep = timestep_range,
                    !! sym(receiver_group), transmitter,
                    fill = list(present = 0))
}

#' Detection ratio data
#'
#' Create absolute (i.e., number of fish) and relative (i.e., proportion of fish) detection ratios per timestep/receiver group.
#'
#' @inheritParams params
#' @return A modified tibble of the detection ratios.
#'
#' @export

detection_ratio <- function(detection_complete, receiver_group = "receiver_group"){

  chk_detection_complete(detection_complete)
  chk_string(receiver_group)

  detection_complete %>%
    group_by(timestep, !! sym(receiver_group)) %>%
    summarise(n = sum(present)) %>%
    mutate(prop = n / sum(n),
           prop = if_else(is.nan(prop), 0, prop))
}
