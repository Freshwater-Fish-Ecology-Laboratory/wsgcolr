#' Add viridis palette to receiver group by rkm
#'
#' @inheritParams params
#' @return A vector of colours
#'
#' @export
rkm_colour <- function(x){
  max_rkm <- max(x)
  palette <- scales::viridis_pal()(max_rkm/0.1)
  palette[x*10]
}

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
  station <- station %>% sf::st_transform(sf::st_crs(river))

  bbox <- sf::st_bbox(station)
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])

  ggplot() +
    geom_sf(data = river, lwd = 0.1, color = "black") +
    geom_sf(data = station, aes(color = receiver_group_colour), show.legend = FALSE) +
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

plot_deployment_temporal <- function(deployment, detection = NULL, station_col = "station_name"){

  chk_deployment(deployment)
  # chkor(chk_null(detection), chk_detection(detection))

  gp <-  ggplot(data = deployment) +
    geom_segment(aes(x = date_period_start, y = !! sym(station_col),
                     xend = date_period_end, yend = !! sym(station_col), color = receiver_group_colour),
                 alpha = 1, size = 3.8) +
    scale_color_identity() +
    geom_point(aes(x = date_period_start, y = !! sym(station_col)), pch = '|', lwd = 2) +
    geom_point(aes(x = date_period_end, y = !! sym(station_col)), pch = '|', lwd = 2) +
    labs(x = "Date", y = "Station", color = "Array") +
    NULL

  if(!is.null(detection)){
    gp <- gp + geom_point(data = detection, aes(x = timestep, y = station_name),
                          color = "black", size = 1)

  }
  gp
}

#' Plot Deployment
#'
#' Generic function to plot the deployment periods.
#'
#' @inheritParams params
#' @export
plot_deployment <- function(x, ...) {
  UseMethod("plot_deployment")
}

#' @describeIn plot_deployment method to plot deployment periods from data.frames
#' @inheritParams params
#' @export
#' @examples
#'
#' \dontrun{
#' plot_deployment(deployment, station, river)
#' }
plot_deployment.data.frame <- function(x, station, river, 
                                       detection = NULL, 
                                       station_col = "station_name", ...){
  
  gp_temp <- plot_deployment_temporal(deployment = x,
                                      detection = detection,
                                      station_col = station_col)
  gp_spat <- plot_deployment_spatial(station = station,
                                     river = river)
  
  patchwork::wrap_plots(gp_temp, gp_spat)
  
}

#' @describeIn plot_deployment method to plot deployments fron connection object
#' @inheritParams params
#' @export
#' @examples
#'
#' \dontrun{
#' plot_deployment(con)
#' }
plot_deployment.PqConnection <- function(x, detection = FALSE, ...){
  
  deployment <- db_query_deployment_period(x, collect = TRUE) 
  station <- db_read_station(x)
  deployment <- left_join(deployment, station, "station_id")
  river <- db_read(x, "spatial.canada_reach", sf = TRUE)
  
  detection_station <- NULL
  if(detection){
    detection_station <-  db_query_detection_station(x)
  }

  plot_deployment(x = deployment,
                  station = station,
                  river = river,
                  detection = detection_station,
                  station_col = "station_name")

}
