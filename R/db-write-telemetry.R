#' Write raw VUE detection csv export to db
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_detection <- function(con, file, clean = TRUE){
  
  col_types <- "Tcccccccnncc"
  x <- readr::read_csv(file, col_types = col_types)
  
  if(clean){
    chk::check_names(x, c("Date and Time (UTC)", "Receiver", "Transmitter", "Transmitter Name",
                          "Transmitter Serial", "Sensor Value", "Sensor Unit", "Station Name",
                          "Latitude", "Longitude", "Transmitter Type", "Sensor Precision"))
    
    x <- x %>%
      rename(datetime_utc = `Date and Time (UTC)`,
             receiver = Receiver,
             transmitter = Transmitter,
             transmitter_name = `Transmitter Name`,
             transmitter_serial = `Transmitter Serial`,
             sensor_value = `Sensor Value`,
             sensor_unit = `Sensor Unit`,
             station_name = `Station Name`,
             lat = Latitude,
             lon = Longitude,
             transmitter_type = `Transmitter Type`,
             sensor_precision = `Sensor Precision`)
  }
  
  x$file <- basename(file)
  
  db_write(con = con, table = "telemetry.detection", data = x)
  
}

#' Write transmitter csv file to db
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_transmitter <- function(con, file){
  col_types <- "ccccnnnccinnnccccTTc"
  x <- readr::read_csv(file, col_types = col_types)
  db_write(con = conn, table = "telemetry.transmitter", data = x)
}

#' Write receiver csv to db
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_receiver <- function(con, x){
  col_types <- "cc"
  x <- readr::read_csv(file, col_types = col_types)
  db_write(con = conn, table = "telemetry.receiver", data = x)
}

#' Write deployment to db
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_deployment <- function(con, x){
  x <- x %>% select(station_id, receiver, date_deployment, activity,
                    download, missing, moved, battery_dead, comment)
  db_write(con = con, table = "telemetry.deployment", data = x)
}

#' Write temperature deployment to db
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_temperature_deployment <- function(con, x){
  x <- x %>% 
    select(station_id, date_deployment, templogger_id,
                    temp_download, temp_redeploy, comment_temp) %>%
    rename(comment = comment_temp,
           logger_id = templogger_id,
           download = temp_download,
           redeploy = temp_redeploy)
  
  db_write(con = con, table = "environmental.temperature_deployment", data = x)
}



