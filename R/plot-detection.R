reference_rkms <- function(){
  tibble::tibble(label = c("HLK Dam", "Kinnaird", "Genelle", "Trail", "Waneta"), rkm = c(0.1, 15, 25, 40, 56))
}

#' Plot Detection Path
#'
#' Generic function to plot the detection paths.
#'
#' @inheritParams params
#' @export
plot_detection_path <- function(x, ...) {
  UseMethod("plot_detection_path")
}

#' @describeIn plot_detection_path method to plot detection paths from data.frames
#' @param lims_x A vector of class 'Date' containing x-axis limits.
#' @param lims_y A vector of the numeric y-axis limits.
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' @examples
#' \dontrun{
#' lims_x = c(min(deployment$date_deployment), max(deployment$date_last_download))
#' lims_y = c(0, 56)
#' plot_detection_path(detection_event, deployment)
#' }

plot_detection_path.data.frame <- function(x, deployment, 
                                           reference_rkm = reference_rkms(),
                                receiver_group = "receiver_group", 
                                receiver_group_rkm = "receiver_group_rkm",
                                lims_x = range(x$timestep), 
                                lims_y = range(x$receiver_group_rkm), 
                                ...){

  # chk_detection_event(detection_event)
  # chk_deployment(deployment)
  # chk_reference_rkm(reference_rkm)
  # 
  # chk_is(lims_x, "Date")
  # chk_length(lims_x, 2L)
  # chk_is(lims_y, "numeric")
  # chk_length(lims_y, 2L)

  ggplot(data = x, aes(x = timestep, y = !! sym(receiver_group_rkm))) +
    geom_segment(data = deployment, aes(x = date_period_start, y = !! sym(receiver_group_rkm),
                                        xend = date_period_end, yend = !! sym(receiver_group_rkm)),
                 alpha = 1, size = 3.8, color = "#EDEDED") +
    geom_line(aes(group = path)) +
    geom_point(aes(color = receiver_group_colour), size = 1.5) +
    scale_color_identity() +
    labs(x = 'Date', y = 'Rkm', color = "Array") +
    coord_cartesian(ylim = lims_y, xlim = lims_x) +
    geom_hline(data = reference_rkm, aes(yintercept = rkm), linetype = 'dotted') +
    geom_text(data = reference_rkm, aes(label = label, x = lims_x[1], y = rkm),
              vjust = -0.5, hjust = 0.1, size = 3.5) +
    theme_bw()

}

#' @describeIn plot_detection_path method to plot detection paths from data.frames
#' @param lims_x A vector of class 'Date' containing x-axis limits.
#' @param lims_y A vector of the numeric y-axis limits.
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' @examples
#' \dontrun{
#' lims_x = c(min(deployment$date_deployment), max(deployment$date_last_download))
#' lims_y = c(0, 56)
#' plot_detection_path(con, transmitter_id = "A69-1303-10060", lims_x = lims_x, lims_y = lims_y)
#' }
plot_detection_path.PqConnection <- function(x, 
                                             transmitter_id = "A69-1303-10070",
                                             max_absence = 96,
                                             lims_x = c(as.Date("2010-01-01"), as.Date("2020-01-01")),
                                             lims_y = c(0, 56), ...){
  
  detection <- db_query_detection_timestep(x, timestep = "week", collect = FALSE)
  
  if(length(transmitter_id))
    detection <- detection %>%
    filter(transmitter %in% transmitter_id)
  
  detection <- collect(detection)
  receiver_group <- db_read_receiver_group(x) %>%
    select(receiver_group, receiver_group, receiver_group_colour)
  station <- db_read_station(x, sf = FALSE)
  detection_event <- detection_event(detection, receiver_group = "receiver_group", max_absence = max_absence) %>%
    left_join(receiver_group, by = "receiver_group")
  deployment <- db_query_deployment_period(x) %>%
    left_join(station, "station_id") %>%
    left_join(receiver_group, "receiver_group")

  plot_detection_path.data.frame(x = detection_event, 
                                 deployment = deployment,
                                 reference_rkm = reference_rkms(), 
                                 receiver_group = "receiver_group",
                                 receiver_group_rkm = "receiver_group_rkm",
                                 lims_x = lims_x, lims_y = lims_y)
  
}
