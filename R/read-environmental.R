get_logger <- function(x){
  stringr::str_split_fixed(x[1,], "-", n = 4)[,4]
}
get_station <- function(x){
  gsub(" ", "", stringr::str_split_fixed(x[2,], ":", n = 2)[,2])
}

read_csv_quiet <- function(x, ...){
  suppressWarnings(suppressMessages(readr::read_csv(x, ...))) 
}

#' Read weird temperature file and extract logger and station
#'
#' @inheritParams params
#' @return A tibble.
#'
#' @export
read_file_temperature <- function(x, tz_data = "Etc/GMT+8"){
  dat <- read_csv_quiet(x, skip = 8, col_names = c("date", "time", "temperature_c"))
  full <- read_csv_quiet(x)
  logger <- get_logger(full)
  station <- get_station(full)
  dat <- dat %>%
    transmute(datetime_pst = dttr2::dtt_date_add_time(date, time, tz = tz_data),
              temperature_c = temperature_c,
              logger_id = logger,
              station = station)
  
  dat
}

#' read discharge csv queried from Poisson db and prepare for wsgcolr db
#'
#' @inheritParams params
#' @return A tibble
#'
#' @export
read_file_discharge_poisson <- function(file, tz_data = "Etc/GMT+8"){
  x <- read_csv_quiet(file)
  
  x <- x %>%
    transmute(datetime_pst = lubridate::force_tz(.data$DateTime, tzone = tz_data) ,
              discharge_cms = as.numeric(.data$Corrected),
              station_id = as.character(.data$Station),
              estimation_status = as.character(.data$Status)) %>%
    filter(!is.na(.data$discharge_cms)) %>%
    filter(!is.na(.data$datetime_pst))
  
  x  
}

#' read discharge csv from border flows and prepare for wsgcolr db
#'
#' @inheritParams params
#' @return A tibble
#'
#' @export
read_file_discharge_border <- function(file, tz_data = "Etc/GMT+8"){
  x <- read_csv_quiet(file)
  
  x <- x %>%
    transmute(datetime_pst = lubridate::force_tz(lubridate::ymd_hm(.data$`Start Date`), tzone = tz_data), 
              station_id = "US_CAN",
              discharge_cms = as.numeric(.data$`US Border (CMS)`),
              estimation_status = NA_character_) %>%
    filter(!is.na(.data$discharge_cms)) %>%
    filter(!is.na(.data$datetime_pst))
  
  x
}

#' read raw VUE detection csv export and prepare for db
#'
#' @inheritParams params
#' @return A tibble
#'
#' @export
read_file_detection <- function(file, clean = TRUE){
  
  col_types <- "Tcccccccnncc"
  x <- readr::read_csv(file, col_types = col_types)
  
  if(clean){
    chk::check_names(x, c("Date and Time (UTC)", "Receiver", "Transmitter", "Transmitter Name",
                          "Transmitter Serial", "Sensor Value", "Sensor Unit", "Station Name",
                          "Latitude", "Longitude", "Transmitter Type", "Sensor Precision"))
    
    x <- x %>%
      rename(datetime_utc = .data$`Date and Time (UTC)`,
             receiver = .data$Receiver,
             transmitter = .data$Transmitter,
             transmitter_name = .data$`Transmitter Name`,
             transmitter_serial = .data$`Transmitter Serial`,
             sensor_value = .data$`Sensor Value`,
             sensor_unit = .data$`Sensor Unit`,
             station_name = .data$`Station Name`,
             lat = .data$Latitude,
             lon = .data$Longitude,
             transmitter_type = .data$`Transmitter Type`,
             sensor_precision = .data$`Sensor Precision`)
  }
  
  x$file <- basename(file)
  x
}

#' Read files using reader function and bind together
#'
#' @inheritParams params
#' @return A tibble.
#'
#' @export
read_files <- function(x, fun = read_temperature_file, quiet = FALSE){
  df <- purrr::map_df(x, function(y) {
    fun(y)
  })
  df
}