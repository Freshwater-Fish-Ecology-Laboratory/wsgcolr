pqgeom_sf <- function(x, geometry = "geom"){
  x %>%
    mutate(geom = st_as_sfc( !! sym(geom))) %>%
    st_as_sf()
}