#' Get complete detection data (i.e. filling in absent receiver_group/timestep)
#'
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
    arrange(transmitter, timestep, receiver_group) %>%
    tidyr::complete(timestep = timestep_range,
                    receiver_group, transmitter,
                    fill = list(present = 0)) %>%
    select(transmitter, receiver_group, timestep, present)
}

#' Detection ratio data
#'
#' Create absolute (i.e., number of fish) and relative (i.e., proportion of fish) 
#' detection ratios per timestep/receiver group.
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