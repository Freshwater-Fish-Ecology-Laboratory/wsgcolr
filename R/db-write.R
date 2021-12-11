#' Write data to database connection
#'
#' @inheritParams params
#' @return The modified database.
#'
#' @export
db_write <- function(con, table, data, append = TRUE, ...){
  schema <- split_schema(table)
  table = split_table(table)
  table_id <- DBI::Id(schema = schema, table = table)
  dbWriteTable(con, name = table_id, value = data, append = append, ...)
}

