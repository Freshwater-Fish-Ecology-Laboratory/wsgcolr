% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chk.R
\name{chk_station}
\alias{chk_station}
\title{Check station data}
\usage{
chk_station(station)
}
\arguments{
\item{station}{A sf object of the station point locations containing column 'array' (for color coding). Geometry column must inherit class 'sfc_POINT' or 'sfc_MULTIPOINT'.}
}
\value{
A flag.
}
\description{
Check station data
}
