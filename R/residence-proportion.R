#' Create residence proportion within timesteps
#' 
#' Input should be output of `residence_event()` function with `squash = TRUE`. 
#' Residence proportion is calculated for each transmitter, receiver_group, 
#' and timestep where fish is present (i.e., from residence events). 
#' If `combine_transmitters = TRUE`, mean residence proportion is calculated for 
#' all transmitters.
#' 
#' @inheritParams params
#' @return A tibble.
#'
#' @export
residence_proportion <- function(x, timestep = "week", combine_transmitters = FALSE){
  
  # for each transmitter and event split into timesteps (one timestep per row)
  # and calculate duration of overlap between timestep interval and intersected event interval
  x_grp <- group_split(x, transmitter, event)
  res <- purrr::map_df(x_grp, function(x){
    x <- tibble(transmitter = x$transmitter, 
                receiver_group = x$receiver_group,
                timestep_start = seq_event(x$event_start, x$event_end, timestep),
                timestep_interval = tstep_interval(x$event_start, x$event_end, timestep),
                duration = duration_interval(lubridate::interval(x$event_start, x$event_end), 
                                             timestep_interval)) 
    x
  }) 
  
  ## in the case where multiple events intersect single timestep, group by timestep and sum duration
  res <- res %>%
    group_by(transmitter, receiver_group, timestep_start, timestep_interval) %>%
    summarize(duration = sum(duration)) %>%
    ungroup() %>%
    mutate(residence_proportion = duration/lubridate::time_length(timestep_interval)) %>%
    select( -duration) %>%
    arrange(transmitter, timestep_start)
  
  if(combine_transmitters)
    res <- res %>%
    group_by(receiver_group, timestep_start, timestep_interval) %>%
    summarize(residence_proportion = mean(residence_proportion)) %>%
    ungroup()
  
  res
  
}

#' Complete residence proportion
#' 
#' Input should be output of `residence_proportion()`. 
#' Possible absent timesteps are determined from first and last detection for each 
#' transmitter. Possible absent receiver_groups are determined from factor levels of receiver_group column.
#' 
#' @inheritParams params
#' @return A tibble.
#'
#' @export
residence_complete <- function(x, timestep){
  
  res_grp <- group_split(x, transmitter)
  res_cplt <- purrr::map_df(res_grp, function(x){
    
    dtseq <- seq(min(x$timestep_start), max(x$timestep_start), by = timestep)
    x <- x %>% tidyr::complete(timestep_start = dtseq,
                               receiver_group,
                               transmitter,
                               fill = list(residence_proportion = 0)) 
    
  })
  
  res_cplt <- res_cplt %>%
    mutate(residence_presence = if_else(residence_proportion == 0, 0, 1)) %>%
    select(transmitter, receiver_group, timestep_start, 
           residence_proportion, residence_presence) %>%
    arrange(transmitter, timestep_start, receiver_group)
  
  res_cplt
  
}