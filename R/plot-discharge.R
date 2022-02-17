#' Plot Deployment
#'
#' Plot discharge by station.
#'
#' @inheritParams params
#' @export
plot_discharge <- function(x, by = "station_id", ...){
  
  gp <- ggplot(data = x, aes(x = datetime_discharge, y = discharge_cms)) +
    geom_point() +
    geom_line() 
  
  if(length(by)){
    gp <- gp +
      facet_wrap(vars(.data[[by]]))
  }
  
  gp
}

