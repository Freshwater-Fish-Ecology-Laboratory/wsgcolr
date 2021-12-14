pqgeom_sf <- function(x, geometry = "geom"){
  x %>%
    mutate(geom = st_as_sfc( !! sym(geom))) %>%
    st_as_sf()
}

timezone_pst <- function(x, datetime = "datetime_pst", tz = "Etc/GMT+8"){
  x[[datetime]] <- lubridate::force_tz(x[[datetime]], tzone = tz)
  x
}