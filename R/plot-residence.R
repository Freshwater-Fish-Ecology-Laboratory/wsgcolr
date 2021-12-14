#' Plot residence event
#'
#' Plot resident events of a single transmitter.
#'  Optionally provide resident_path as geom_line and deployments as grey geom_segments.
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' \dontrun{
#' x <- residence_event(detection)
#' plot_residence_event(x)
#' }
plot_residence_event <- function(x, 
                                 residence_path = NULL, 
                                 deployment = NULL, 
                                 reference_rkm = reference_rkms(),
                                 datetime = "datetime_pst",
                                 receiver_group = "receiver_group", 
                                 receiver_group_rkm = "receiver_group_rkm",
                                 lims_x = range(x$event_start), 
                                 lims_y = levels(x$receiver_group_rkm),
                                 ...){
  
  # chk_detection_event(detection_event)
  # chk_deployment(deployment)
  # chk_reference_rkm(reference_rkm)
  # 
  # chk_is(lims_x, "Date")
  # chk_length(lims_x, 2L)
  # chk_is(lims_y, "numeric")
  # chk_length(lims_y, 2L)
  
  gp <- ggplot(data = x, aes(x = !! sym(datetime), 
                                     y = !! sym(receiver_group_rkm)))
  
  if(length(deployment))
    gp <- gp + 
      geom_segment(data = deployment, aes(x = as.POSIXct(date_period_start), y = !! sym(receiver_group_rkm),
                                          xend = as.POSIXct(date_period_end), yend = !! sym(receiver_group_rkm)),
                   alpha = 1, size = 4, color = "#EDEDED")
  
  if(length(residence_path))
    gp <- gp +
      geom_line(data = residence_path, aes(group = event))
  
  gp +
    geom_segment(aes(color = receiver_group_colour,
                     x = event_start, xend = event_end,
                     y = !! sym(receiver_group_rkm), 
                     yend = !! sym(receiver_group_rkm)), size = 3.5) +
    scale_color_identity() +
    labs(x = 'Date', y = 'Rkm', color = "Array") +
    coord_cartesian(ylim = lims_y, xlim = lims_x) +
    geom_hline(data = reference_rkm, aes(yintercept = rkm), linetype = 'dotted') +
    geom_text(data = reference_rkm, aes(label = label, x = lims_x[1], y = rkm),
              vjust = -0.5, hjust = 0.1, size = 3.5) +
    theme_bw()

}

#' Plot residence proportion as heatmap
#'
#' Input data should be output of `residence_proportion()` function.
#' Residence proportion calculations are derived from residence events 
#' and specified timestep. 
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' \dontrun{
#' event <- residence_event(detection)
#' x <- residence_proportion_complete(event)
#' plot_residence(x)
#' }
plot_residence_proportion <- function(x, size = 0.3){
  
  ggplot(data = x) +
    geom_tile(aes(x = timestep_start, y = receiver_group, 
                  fill = residence_proportion, 
                  color = residence_proportion), size = size) +
    scale_fill_viridis_c() + 
    scale_colour_viridis_c()
}

