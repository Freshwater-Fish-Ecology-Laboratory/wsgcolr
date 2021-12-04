is_try_error <- function(x){
  inherits(x, "try-error")
}

execute <- function(conn, sql){
  DBI::dbExecute(conn, sql)
}


set_schema <- function(conn, schema) {
  sql <- "SET SCHEMA ?schema"
  query <- DBI::sqlInterpolate(conn, sql, schema = schema)
  DBI::dbExecute(conn, query)
  invisible()
}

#' Connect to database
#'
#' @inheritParams params
#' @return A PostgreSQL connection object.
#'
#' @export
db_connect <- function(dbname = getOption("dbname"), schema = NULL, pool = FALSE) {
  
  config <- config::get(dbname)
  fun <- if(pool) pool::dbPool else DBI::dbConnect
  conn <- fun(
    RPostgres::Postgres(),
    user = config$user,
    password = config$password,
    host = config$host,
    port = config$port,
    dbname = config$dbname
  )
  
  if(!is.null(schema)) set_schema(conn, schema)
  conn
}

#' Disconnect from database
#'
#' @inheritParams params
#' @return A PostgreSQL connection object.
#'
#' @export
db_disconnect <- function(conn){
  DBI::dbDisconnect(conn)
}
