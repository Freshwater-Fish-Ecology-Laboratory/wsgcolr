#' Plot station map
#'
#' Plot receiver station locations in Columbia and Kootenay River.
#'
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' @examples
#' \dontrun{
#' plot_deployment_spatial(station, river)
#' }

plot_deployment_spatial <- function(station, river){

  chk_station(station)
  chk_river(river)

  bbox <- sf::st_bbox(station)
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])

  ggplot() +
    geom_sf(data = river, lwd = 0.1, color = "black") +
    geom_sf(data = station, aes(color = array_color), show.legend = FALSE) +
    scale_color_identity() +
    coord_sf(xlim = xlim, ylim = ylim) +
    ggspatial::annotation_scale(height = unit(0.09, "cm"), text_cex = 0.5) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) +
    NULL
}

#' Plot deployment
#'
#' Plot receiver deployments over time.
#' Optionally provide detection data to visualize detections overlapping deployment periods.
#'
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' @examples
#' \dontrun{
#' plot_deployment_temporal(deployment)
#' }

plot_deployment_temporal <- function(deployment, detection = NULL){

  chk_deployment(deployment)
  chkor(chk_null(detection), chk_detection_timestep(detection))

  gp <-  ggplot(data = deployment) +
    geom_segment(aes(x = date_deployment, y = station_id,
                     xend = date_last_download, yend = station_id, color = array_color),
                 alpha = 1, size = 3.8) +
    scale_color_identity() +
    geom_point(aes(x = date_deployment, y = station_id), pch = '|', lwd = 2) +
    geom_point(aes(x = date_last_download, y = station_id), pch = '|', lwd = 2) +
    labs(x = "Date", y = "Station", color = "Array") +
    NULL

  if(!is.null(detection)){
    gp <- gp + geom_point(data = detection, aes(x = timestep, y = station_id),
                          color = "black", size = 1)

  }
  gp
}

#' Plot receiver coverage
#'
#' Plot combination of station map and receiver deployment over time.
#'
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' @examples
#'
#' \dontrun{
#' plot_deployment(station, river, deployment)
#' }

plot_deployment <- function(station, river, deployment, detection = NULL){

  gp_temp <- plot_deployment_temporal(deployment = deployment,
                                   detection = detection)
  gp_spat <- plot_deployment_spatial(station = station,
                             river = river)

  patchwork::wrap_plots(gp_temp, gp_spat)

}
