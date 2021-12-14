test_that("plotting funs work", {
  # con <- db_connect(dbname = "wsgcolr")
  # station <- db_read_station(con)
  # river <- db_read(con, "spatial.canada_reach", sf = TRUE)
  # deployment <- db_query_deployment_period(con) %>%
  #   left_join(station, "station_id")
  #   
  # wsgcolr::plot_deployment(deployment, station = station, 
  #                          river = river, station_col = "station_name")
  # 
  # wsgcolr::plot_deployment(con, detection = TRUE)
  # 
  # wsgcolr::plot_detection_path(con)
  # 
  # wsgcolr::plot_discharge(con)
  # 
  # tags <- db_query_capture_tidy(con) %>%
  #   filter(forklength_cm > 200) %>%
  #   pull(transmitter_id)
  # 
  # # tags <- c("A69-1303-10063")
  # detection <- db_query_detection_tidy(con, collect = FALSE) %>%
  #   filter(transmitter %in% tags) %>%
  #   collect()
  # 
  # x <- residence_event(detection)
  # receiver_group <- db_read_receiver_group(con)
  # station <- db_read_station(con, sf = FALSE)
  # residence_event <- x %>%
  #   left_join(receiver_group, "receiver_group")
  # residence_path <- residence_event(detection, squash = FALSE, ignore_movement = TRUE)
  # deployment <- db_query_deployment_period(con) %>%
  #   left_join(station, "station_id")
  # 
  # plot_residence_event(residence_event, residence_path, deployment)
  
  
})
