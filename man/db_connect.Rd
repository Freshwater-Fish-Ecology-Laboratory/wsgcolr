% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db-utils.R
\name{db_connect}
\alias{db_connect}
\title{Connect to database}
\usage{
db_connect(dbname = getOption("dbname"), schema = NULL)
}
\arguments{
\item{dbname}{A string of the database name.}

\item{schema}{A string of the schema name.}
}
\value{
A PostgreSQL connection object.
}
\description{
Connect to database
}
