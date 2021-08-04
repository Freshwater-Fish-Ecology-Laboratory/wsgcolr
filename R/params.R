#' Parameter Descriptions for wsgcolr functions
#'
#' @param detection_event A tibble of detection path data returned from `wsgcolr::detection_event()`containing columns 'transmitter_id', 'timestep', 'path', 'array', 'array_rkm'.
#' @param detection_timestep A tibble of aggregated detection data returned from `wsgcolr::detection_timestep()` containing columns 'transmitter_id' and 'timestep'. If `NULL`, argument is ignored.
#' @param detection_complete A tibble of complete detection data returned from `wsgcolr::detection_complete()` containing columns 'transmitter_id' and 'timestep'.
#' @param detection_ratio A tibble of detection rtio data returned from `wsgcolr::detection_ratio()` containing columns 'timestep', 'n' and 'prop'.
#' @param detection A tibble of raw detection data containing columns 'transmitter_id' and 'datetime_utc'.
#' @param receiver_group A character string of the column containing receiver group (e.g., station, zone, array).
#' @param receiver_group_rkm A character string of the column containing receiver group rkm.
#' @param reference_rkm A tibble of reference rkm locations containing columns 'label' and 'rkm'.
#' @param timestep A character string of the timestep. Acceptable values are those accepted by `lubridate::floor_date()`: second, minute, hour, day, week, month, bimonth, quarter, season, halfyear and year.
#' @param max_absence A number of the maximum hours without detection for new event to occur.
#' @param squash A flag indicating whether to reduce detection events to first and last timesteps.
#' @param deployment A tibble of the receiver deployments containing columns 'date_deployment', 'date_last_download', 'station', 'array', 'array_rkm'.
#' @param station A sf object of the station point locations containing column 'array' (for color coding). Geometry column must inherit class 'sfc_POINT' or 'sfc_MULTIPOINT'.
#' @param river A sf object of the river polygon. Geometry column must inherit class 'sfc_POLYGON' or 'sfc_MULTIPOLYGON'.
#' @keywords internal
#' @name params
NULL
