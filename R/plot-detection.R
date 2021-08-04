#' Plot detection paths
#'
#' Plot Columbia River White Sturgeon movement paths from VR2 receiver detection data.
#'
#'
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
#' plot_detection_path(detection, deployment, reference_rkm)
#' }

plot_detection_path <- function(detection_event, deployment, reference_rkm,
                                receiver_group, receiver_group_rkm,
                                lims_x, lims_y){

  chk_detection_event(detection_event)
  chk_deployment(deployment)
  chk_reference_rkm(reference_rkm)

  chk_is(lims_x, "Date")
  chk_length(lims_x, 2L)
  chk_is(lims_y, "numeric")
  chk_length(lims_y, 2L)

  ggplot(data = detection_event, aes(x = timestep, y = !! sym(receiver_group_rkm))) +
    geom_segment(data = deployment, aes(x = date_deployment, y = !! sym(receiver_group_rkm),
                                        xend = date_last_download, yend = !! sym(receiver_group_rkm)),
                 alpha = 1, size = 3.8, color = "#EDEDED") +
    geom_line(aes(group = path)) +
    geom_point(aes(color = !! sym(receiver_group)), size = 1.3) +
    labs(x = 'Date', y = 'Rkm', color = "Array") +
    lims(x = lims_x,
         y = lims_y) +
    geom_hline(data = reference_rkm, aes(yintercept = rkm), linetype = 'dotted') +
    geom_text(data = reference_rkm, aes(label = label, x = lims_x[1], y = rkm),
              vjust = -0.5, hjust = 0.1, size = 3.5)

}
