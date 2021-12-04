write_discharge_poisson <- function(con, file){
  x <- readr::read_csv(file,
                       col_types = "Tcccccccnncc")
  
  chk::check_names(x, c("Date and Time (UTC)", "Receiver", "Transmitter", "Transmitter Name",
                        "Transmitter Serial", "Sensor Value", "Sensor Unit", "Station Name",
                        "Longitude", "Transmitter Type", "Sensor Precision"))
  
  names(x) <- c('datetime_utc', 'receiver', 'transmitter', 
                'transmitter_name', 'transmitter_serial', 
                'sensor_value', 'sensor_unit', 'station_name', 
                'lat', 'lon', 'transmitter_type', 'sensor_precision')
  
  x$file <- basename(file)
  
  db_write(con = con, schema = "telemetry", table = "detection", data = x)
  
}

write_discharge_border <- function(con, file){
  
}