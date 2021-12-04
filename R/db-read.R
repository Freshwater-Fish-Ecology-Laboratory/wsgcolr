split_schema <- function(x){
  strsplit(x, ".", fixed = TRUE)[[1]][1]
}

split_table <- function(x){
  strsplit(x, ".", fixed = TRUE)[[1]][2]
}

#' Read table from database connection
#'
#' @inheritParams params
#' @return A data.frame of the data.
#'
#' @export
db_read <- function(conn, table, sf = FALSE){
  schema <- split_schema(table)
  table = split_table(table)
  table_id <- DBI::Id(schema = schema, table = table)
  if(!sf){
    x <- tibble::as_tibble(DBI::dbReadTable(conn, name = table_id))  
  } else {
    x <- sf::st_read(dsn = conn, layer = table_id)
  }
  x
}

#' Read detection_tidy_weekly View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_station <- function(conn){
  db_read(con, "telemetry.station", sf = TRUE) %>%
    mutate(receiver_group_colour = wsgcolr::rkm_colour(receiver_group_rkm)) %>%
    mutate(station_name = forcats::fct_rev(forcats::fct_reorder(station_name, rkm)))
}

#' Read detection_tidy_weekly View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_deployment_period <- function(conn){
  station <- db_read_station(conn)
  db_read(conn, "telemetry.deployment_period") %>%
    left_join(station, "station_id")
}

#' Read detection_tidy_weekly View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_detection_weekly <- function(conn){
  db_read(conn, "telemetry.detection_tidy_weekly")
}

#' Read detection_tidy_daily View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_detection_daily <- function(conn){
  db_read(conn, "telemetry.detection_tidy_daily")
}

#' Read detection_tidy_daily View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_detection_simple <- function(con){
  detection <- tbl(con, dbplyr::in_schema("telemetry", "detection_clean"))
  detection %>%
    mutate(timestep = as.Date(sql("date_trunc('week', datetime_utc)"))) %>%
    distinct(timestep, station_id) %>%
    collect() %>%
    left_join(station, "station_id")
}