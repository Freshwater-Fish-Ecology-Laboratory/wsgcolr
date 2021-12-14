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
  station <- db_read(con, "telemetry.station", sf = sf) 
  receiver_group <- db_read_receiver_group(con)
  station %>%
    left_join(receiver_group, "receiver_group") %>%
    mutate(station_name = forcats::fct_rev(forcats::fct_reorder(.data$station_name, .data$rkm)),
           station_id = forcats::fct_rev(forcats::fct_reorder(.data$station_id, .data$rkm)),
           receiver_group = forcats::fct_rev(forcats::fct_reorder(.data$receiver_group, .data$receiver_group_rkm)))
}

#' Read receiver_group table and add receiver_group_colour
#'
#' @inheritParams params
#' @return A tibble of the summarized detection data.
#'
#' @export
db_read_receiver_group <- function(con){
  db_read(con, "telemetry.receiver_group") %>%
    mutate(receiver_group_colour = wsgcolr::rkm_colour(.data$receiver_group_rkm)) %>%
    mutate(receiver_group = forcats::fct_rev(forcats::fct_reorder(.data$receiver_group, .data$receiver_group_rkm)))
}

