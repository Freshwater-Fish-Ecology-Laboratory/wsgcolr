#' Write discharge csv queried from Poisson db
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_discharge_poisson <- function(con, file, tz_data = "Etc/GMT+8"){
  x <- readr::read_csv(file)
  
  y <- x %>%
    transmute(datetime_pst = lubridate::force_tz(.data$DateTime, tzone = tz_data) ,
              discharge_cms = as.numeric(.data$Corrected),
              station_id = as.character(.data$Station),
              estimation_status = as.character(.data$Status)) %>%
    filter(!is.na(.data$discharge_cms)) %>%
    filter(!is.na(.data$datetime_pst))
  
  db_write(con = con, table = "environmental.discharge", data = x)
  
}

#' Write discharge csv from border flows
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_discharge_border <- function(con, file, tz_data = "Etc/GMT+8"){
  x <- readr::read_csv(file)
  
  x <- x %>%
    transmute(datetime_pst = lubridate::force_tz(.data$`Start Date`, tzone = tz_data), 
              station_id = "US_CAN",
              discharge_cms = as.numeric(.data$`US Border (CMS)`),
              estimation_status = NA_character_) %>%
    filter(!is.na(.data$discharge_cms)) %>%
    filter(!is.na(.data$datetime_pst))
  
  db_write(con = con, table = "environmental.discharge", data = x)
  
}
