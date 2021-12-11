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
read_temperature_file <- function(x, tz = "PST8PDT"){
  dat <- read_csv_quiet(x, skip = 8, col_names = c("date", "time", "temperature_c"))
  full <- read_csv_quiet(x)
  logger <- get_logger(full)
  station <- get_station(full)
  dat <- dat %>%
    transmute(datetime_temperature = dttr2::dtt_adjust_tz(dttr2::dtt_date_add_time(date, time, tz = tz)),
              temperature_c = temperature_c,
              logger_id = logger,
              station = station)
  
  dat
}

#' Read weird temperature files and extract logger and station
#'
#' @inheritParams params
#' @return A tibble.
#'
#' @export
read_temperature_files <- function(x, quiet = FALSE){
  df <- purrr::map_df(x, function(y) {
    read_temperature_file(y)
  })
  
  if(!quiet){
    print(unique(df$logger_id))
    print(unique(df$station))
    print(head(df))
  }
  
  df
}