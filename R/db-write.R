# leave open to sf method (i.e. do not explicitly call DBI)
db_write <- function(conn, schema, table, data, append = TRUE){
  table_id <- DBI::Id(schema = schema, table = table)
  dbWriteTable(conn, name = table_id, value = data, append = append)
}

