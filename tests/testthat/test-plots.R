test_that("plotting funs work", {
  con <- db_connect()
  station <- db_read_station(con)
  river <- db_read(con, "spatial.canada_reach", sf = TRUE)
  deployment <- db_query_deployment_period(con) %>%
    left_join(station, "station_id")
    
  wsgcolr::plot_deployment(deployment, station = station, 
                           river = river, station_col = "station_name")
  
  wsgcolr::plot_deployment(con, detection = TRUE)
  
  wsgcolr::plot_detection_path(con)
  
  # wsgcolr::plot_discharge(con)
  
  
})
