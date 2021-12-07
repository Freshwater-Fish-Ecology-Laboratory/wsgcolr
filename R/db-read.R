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
db_read <- function(con, table, sf = FALSE, collect = TRUE){
  schema <- split_schema(table)
  table = split_table(table)
  if(!sf){
    x <- tbl(con, dbplyr::in_schema(schema = schema, table = table))
    if(collect){
      x <- collect(x)
    } 
  } else {
    x <- sf::st_read(dsn = con, layer = DBI::Id(schema = schema, table = table))
  }
  x
}

#' Read detection_tidy_weekly View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_station <- function(con, sf = TRUE){
  db_read(con, "telemetry.station", sf = sf) %>%
    mutate(receiver_group_colour = wsgcolr::rkm_colour(receiver_group_rkm)) %>%
    mutate(station_name = forcats::fct_rev(forcats::fct_reorder(station_name, rkm)))
}

#' Read receiver_group table and add receiver_group_colour
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_receiver_group <- function(con){
  db_read(con, "telemetry.receiver_group") %>%
    mutate(receiver_group_colour = wsgcolr::rkm_colour(receiver_group_rkm)) %>%
    mutate(receiver_group = forcats::fct_rev(forcats::fct_reorder(receiver_group, receiver_group_rkm)))
}

#' Read detection_tidy_weekly View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_deployment_period <- function(con){
  station <- db_read_station(con)
  db_read(con, "telemetry.deployment_period") %>%
    left_join(station, "station_id")
}

#' Read detection_tidy_weekly View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_detection_tidy <- function(con, timestep = "week", transmitter_id = NULL){
  chk_subset(timestep, c("week", "day"))
  if(timestep == "week"){
    x <- db_read(con, "telemetry.detection_tidy_weekly", collect = FALSE)
  } else {
    x <- db_read(con, "telemetry.detection_tidy_daily", collect = FALSE)
  }
  
  if(length(transmitter_id)){
    x <- x %>%
      filter(transmitter %in% transmitter_id)
  }
  
  x %>%
    select(transmitter, receiver_group, timestep, ndetects, pittag_id, tag_insertion_date,
           forklength_cm, weight_kg, sex, receiver_group_rkm, receiver_group_flow_zone) %>%
    collect() %>%
    mutate(receiver_group = forcats::fct_rev(forcats::fct_reorder(receiver_group, receiver_group_rkm)))
}

#' Read detection_tidy_daily View
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_detection_simple <- function(con){
  detection <- db_read(con, "telemetry.detection_clean", collect = FALSE)
  station <- db_read_station(con)
  detection %>%
    mutate(timestep = as.Date(sql("date_trunc('week', datetime_utc)"))) %>%
    distinct(timestep, station_id) %>%
    collect() %>%
    left_join(station, "station_id")
}