#' Write discharge csv queried from Poisson db
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_discharge_poisson <- function(con, file, tz_data = "Etc/GMT+8"){
  x <- read_file_discharge_poisson(file)
  db_write(con = con, table = "environmental.discharge", data = x)
  
}

#' Write discharge csv from border flows
#'
#' @inheritParams params
#' @return The modified database
#'
#' @export
db_write_discharge_border <- function(con, file, tz_data = "Etc/GMT+8"){
  x <- read_file_discharge_border(file)
  db_write(con = con, table = "environmental.discharge", data = x)
  
}
