reference_rkms <- function(){
  tibble::tibble(label = c("HLK Dam", "Kinnaird", "Genelle", "Trail", "Waneta"), rkm = c(0.1, 15, 25, 40, 56))
}

#' Plot residence event
#'
#' Plot resident events of a single transmitter.
#'  Optionally provide resident_path as geom_line and deployments as grey geom_segments.
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#' @export
#' @examples
#' \dontrun{
#' x <- residence_event(detection)
#' plot_residence_event(x)
#' }
plot_residence_event <- function(x, 
                                 residence_path = NULL, 
                                 deployment = NULL, 
                                 datetime = "datetime_pst",
                                 receiver_group = "receiver_group", 
                                 receiver_group_rkm = "receiver_group_rkm",
                                 xlims = range(x$event_start), 
                                 ylims = levels(x$mean_rkm),
                                 reference_rkm = reference_rkms(),
                                 ...){
  
  # chk_detection_event(detection_event)
  # chk_deployment(deployment)
  # chk_reference_rkm(reference_rkm)
  # 
  # chk_is(lims_x, "Date")
  # chk_length(lims_x, 2L)
  # chk_is(lims_y, "numeric")
  # chk_length(lims_y, 2L)
  
  gp <- ggplot(data = x, aes(x = !! sym(datetime)))
  
  y <- "mean_rkm"
  if(length(deployment)){
    y <- receiver_group_rkm
    gp <- gp + 
      geom_segment(data = deployment, aes(x = as.POSIXct(date_period_start), y = !! sym(y),
                                          xend = as.POSIXct(date_period_end), yend = !! sym(y)),
                   alpha = 1, size = 4, color = "#EDEDED")
  }
    
  
  if(length(residence_path)){
    y <- receiver_group_rkm
    gp <- gp +
      geom_line(data = residence_path, aes(group = event, y = !! sym(y)))
  }
    
  gp +
    geom_segment(aes(x = event_start, 
                     xend = event_end,
                     y = !! sym(y), 
                     yend = !! sym(y), 
                     color = receiver_group), 
                 size = 3.5) +
    scale_color_discrete(drop = FALSE, guide = guide_legend(reverse = TRUE)) +
    scale_y_reverse() +
    labs(x = 'Date', y = 'Rkm', color = "Receiver Group") +
    coord_cartesian(ylim = ylims, xlim = xlims) +
    geom_hline(data = reference_rkm, aes(yintercept = rkm), linetype = 'dotted') +
    geom_text(data = reference_rkm, aes(label = label, x = xlims[1], y = rkm),
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
#' @export
#' @examples
#' \dontrun{
#' event <- residence_event(detection)
#' x <- residence_proportion_complete(event)
#' plot_residence(x)
#' }
plot_residence_proportion <- function(x, 
                                      xlab = "Timestep", 
                                      ylab = "Receiver Group", 
                                      legend_lab = "Residence\nProportion", size = 0.3){
  
  ggplot(data = x) +
    geom_tile(aes(x = timestep_start, y = receiver_group, 
                  fill = residence_proportion, 
                  color = residence_proportion), size = size) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_viridis_c() + 
    scale_colour_viridis_c() +
    labs(x = xlab, y = ylab, colour = legend_lab, fill = legend_lab)
}

