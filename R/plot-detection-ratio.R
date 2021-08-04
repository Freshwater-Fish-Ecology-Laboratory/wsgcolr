plot_detection_ratio <- function(detection_ratio, receiver_group, y, ylab){

  chk_detection_ratio(detection_ratio)
  chk_string(receiver_group)
  chk_string(ylab)

  ggplot(data = detection_ratio, aes(x = timestep, y = !! sym(y), fill = !! sym(receiver_group))) +
    geom_area() +
    labs(x = "Date", y = ylab)
}

#' Plot absolute detection ratio
#'
#' Plot stacked area chart for absolute detection ratios (i.e., number of fish per timestep/receiver group).
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' @examples
#' \dontrun{
#' plot_detection_ratio_absolute(detection_ratio)
#' }

plot_detection_ratio_absolute <- function(detection_ratio, receiver_group = "array"){

  plot_detection_ratio(detection_ratio, receiver_group, y = "n", ylab = "Number of Fish")
}

#' Plot relative detection ratio
#'
#' Plot stacked area chart for relative detection ratios (i.e., proportion of fish per timestep/receiver group).
#'
#' @inheritParams params
#' @return An object of class 'ggplot'.
#'
#' @export
#' @examples
#' \dontrun{
#' plot_detection_ratio_relative(detection_ratio)
#' }

plot_detection_ratio_relative <- function(detection_ratio, receiver_group = "array"){

  plot_detection_ratio(detection_ratio, receiver_group, y = "prop", ylab = "Proportion of Fish")
}
