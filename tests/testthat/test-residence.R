test_that("detection event functions work", {
  
  con <- db_connect()
  
  tags <- db_query_capture_tidy(con) %>%
    filter(forklength_cm > 200) %>%
    pull(transmitter_id) 
  
  # tags <- tags[1:5]
  
  # tags <- c("A69-1303-10063")
  detection <- db_query_detection_tidy(con, collect = FALSE) %>%
    filter(transmitter %in% tags) %>%
    collect()
  
  receiver_groups <- db_read_receiver_group(con)
  
  detection <- detection %>%
    mutate(receiver_group = factor(receiver_group, levels = levels(receiver_groups$receiver_group)))
  
  # x <- residence_event(detection, squash = FALSE)
  # x2 <- residence_event(detection, max_absence = 1000, squash = FALSE)
  # x3 <- residence_event(detection, min_detections = 100, squash = FALSE)
  # x4 <- residence_event(detection, min_duration = 100, squash = FALSE)
  # 
  # ## expect that greater max_absence leads to more event 1 occurrences
  # expect_true(sum(x2$event == 1) > sum(x$event == 1))
  # # expect fewer events when higher min_detections and min_duration
  # expect_true(nrow(x3) < nrow(x))
  # expect_true(nrow(x4) < nrow(x))
  # 
  # x5 <- residence_event(detection, squash = TRUE)
  # expect_true(nrow(x5) < nrow(x))
  # 
  # x6 <- residence_path(detection)
  # expect_true(sum(x6$event == 1) > sum(x$event == 1))
  
  event <- residence_event(detection, squash = TRUE)
  ## need to add calculation of mean rkm/lat/lon weighted by #detections / station
  event1 <- event %>% filter(transmitter == tags[5]) %>%
    left_join(receiver_groups, "receiver_group")
  plot_residence_event(event1)
  
  res_month <- residence_proportion(event, "month")
  res_week <- residence_proportion(event, "week")
  res_day <- residence_proportion(event, "day")
  res_year <- residence_proportion(event, "year")
  
  res_cplt <- residence_complete(res_year, "year")
  
  res_com <- residence_proportion(event, "year", combine_transmitters = TRUE)
  
  plot_residence_proportion(res_com)
  plot_residence_proportion(res_year)
  plot_residence_proportion(res_cplt)
  
  abun <- abundance(event, timestep = "month")
  plot_abundance_proportion(abun)

  ### what about if receiver group had no active deployment during timestep period?
  

  
  
  residence <- readRDS("~/onedrive/data/wsgcolr/laurence_scripts/working_databases/Residence_0920.rds")
  residence2 <- readRDS("~/onedrive/data/wsgcolr/laurence_scripts/working_databases/residence_0919_AllSites.rds")
  abundance <- readRDS("~/onedrive/data/wsgcolr/laurence_scripts/working_databases/Abundance_0920.rds")
  movement <- readRDS("~/onedrive/data/wsgcolr/laurence_scripts/working_databases/Movement_0920.rds")
  
  x <- residence %>%
    filter(id == "A69-1303-10065") %>%
    select(id, yrwk, rkmCh, totalduration, residence_perc)
  
  x2 <- residence2 %>%
    filter(id == "A69-1303-10065") %>%
    select(id, timestep, residence, site, residence_perc)
  
  y <- abundance %>%
    select(yrwk, rkmCh, abun, abundance)
  
  tmp <- x %>%
    group_split(transmitter, receiver_group, event) %>%
    purrr::map_df(function(x){
      timerange <- seq(x$event_start, x$event_end, by = "day") 
      tibble(timestep = timerange,
             event = x$event,
             transmitter = x$transmitter,
             receiver_group = x$receiver_group,
             present = 1)
    }) %>%
    arrange(transmitter, event, timestep)
  
  receiver_groups <- unique(tmp2$receiver_group)
  tmp2 <- tmp %>%
    group_split(transmitter) %>%
    purrr::map_df(function(x){
      x <- x %>% arrange(timestep)
      timerange <- seq(first(x$timestep), last(x$timestep), by = "day") 
      tidyr::complete(x, timestep = timerange,
                      transmitter, receiver_group = receiver_groups, 
                      fill = list(present = 0))
    }) %>%
    arrange(transmitter, timestep)
  
  detection_event %>%
    arrange(transmitter, timestep, receiver_group) %>%
    rowwise() %>%
    tidyr::complete(timestep = seq.Date(as.Date(event_start), as.Date(event_end), 
                                        by = "week"),
                    receiver_group, transmitter,
                    fill = list(present = 0)) %>%
    select(transmitter, receiver_group, timestep, present)
  
  
})
