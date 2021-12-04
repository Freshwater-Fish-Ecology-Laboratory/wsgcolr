chk_dat <- function(x, names){
  chk_is(x, "tbl")
  check_names(x ,names = names)
}

#' Check timestep arg
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export
#'
chk_timestep <- function(timestep){
  chk_subset(timestep, c("second", "minute", "hour", "day", "week", "month", "bimonth", "quarter", "season", "halfyear", "year"))
}

#' Check detection path data
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_detection_event <- function(detection_event){
  chk_dat(detection_event, names =  c("transmitter", "timestep", "path", "event"))
}

#' Check detection timestep data
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_detection_timestep <- function(detection_timestep){
    chk_dat(detection_timestep ,names =  c("transmitter", "timestep"))
}

#' Check detection data
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_detection <- function(detection){
  chk_dat(detection, names =  c("transmitter"))
}

#' Check complete detection data
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_detection_complete <- function(detection_complete){
  chk_dat(detection_complete, names =  c("transmitter", "timestep", "present"))
}

#' Check detection ratio data
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_detection_ratio <- function(detection_ratio){
  chk_dat(detection_ratio, names =  c("timestep", "n", "prop"))
}

#' Check deployment data
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_deployment <- function(deployment){
  chk_dat(deployment, names = c("date_period_start", "date_period_end", "station_id"))
}

#' Check station data
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_station <- function(station){
  chk_is(station, "sf")
  check_names(station, names = c("receiver_group"))
  chkor(chk_is(sf::st_geometry(station), "sfc_POINT"), chk_is(sf::st_geometry(station), "sfc_MULTIPOINT"))
}

#' Check river data
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_river <- function(river){
  chk_is(river, "sf")
  chkor(chk_is(sf::st_geometry(river), "sfc_POLYGON"), chk_is(sf::st_geometry(river), "sfc_MULTIPOLYGON"))
}

#' Check reference rkm locations
#'
#' @inheritParams params
#' @return A flag.
#'
#' @export

chk_reference_rkm <- function(reference_rkm){
  chk_dat(reference_rkm, names = c("label", "rkm"))
}
