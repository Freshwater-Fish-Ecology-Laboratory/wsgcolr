plot_abun <- function(x, xlab, ylab, legend_lab, fill, size){
  ggplot(data = x) +
    geom_tile(aes(x = timestep_start,
                  y = receiver_group, 
                  fill = !! sym(fill), 
                  color = !! sym(fill)), size = size) +
    scale_fill_viridis_c() + 
    scale_colour_viridis_c() +
    labs(x = xlab, y = ylab, fill = legend_lab, color = legend_lab)
}

#' Plot abundance heatmap as proportion of total tagged fish.
#'
#' Input data should be output of `abundance()` function. Abundance 
#' calculations are derived from residence events and specified timestep.
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' \dontrun{
#' event <- residence_event(detection)
#' x <- abundance(event)
#' plot_abundance_proportion(x)
#' }
plot_abundance_proportion <- function(x, xlab = "Timestep", 
                                      ylab = "Receiver Group",
                                      legend_lab = "Abundance\nProportion",
                                      size = 0.3){
  
  plot_abun(x = x, xlab = xlab, ylab = ylab, fill = "abundance_proportion",
            legend_lab = legend_lab, size = size)
}

#' Plot absolute abundance heatmap
#'
#' Input data should be output of `abundance()` function. Abundance 
#' calculations are derived from residence events and specified timestep.
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' \dontrun{
#' event <- residence_event(detection)
#' x <- abundance(event)
#' plot_abundance(x)
#' }
plot_abundance <- function(x, xlab = "Timestep", 
                           ylab = "Receiver Group", 
                           legend_lab = "Abundance", 
                           size = 0.3){
  
  plot_abun(x = x, xlab = xlab, ylab = ylab, fill = "abundance",
            legend_lab = legend_lab, size = size)
  
  
}