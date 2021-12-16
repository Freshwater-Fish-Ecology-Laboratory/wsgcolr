#' Write raw VUE detection csv export to db
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_detection <- function(con, file, clean = TRUE){
  
  x <- read_file_detection(file, clean = clean)
  
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
  db_write(con = con, table = "telemetry.transmitter", data = x)
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
  db_write(con = con, table = "telemetry.receiver", data = x)
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
    select(.data$station_id, .data$date_deployment, .data$templogger_id,
           .data$temp_download, .data$temp_redeploy, .data$comment_temp) %>%
    rename(comment = .data$comment_temp,
           logger_id = .data$templogger_id,
           download = .data$temp_download,
           redeploy = .data$temp_redeploy)
  
  db_write(con = con, table = "environmental.temperature_deployment", data = x)
}



