#' Plot Deployment
#'
#' Generic function to plot discharge by station.
#'
#' @inheritParams params
#' @export
plot_discharge <- function(x, ...) {
  UseMethod("plot_discharge")
}

#' @describeIn plot_discharge method to plot discharge from data.frame
#' @inheritParams params
#' @export
plot_discharge.data.frame <- function(x, by = "station_id"){
  
  gp <- ggplot(data = x, aes(x = datetime_discharge, y = discharge_cms)) +
    geom_point() +
    geom_line() 
  
  if(length(by)){
    gp <- gp +
      facet_wrap(vars(.data[[by]]))
  }
  
  gp
}

#' @describeIn plot_discharge method to plot discharge from db connection
#' @inheritParams params
#' @export
plot_discharge.PqConnection <- function(con, station = c("HLK_ALH", "BRD_BRDS_BRX", 
                                                         "US_CAN", "BIR_QR"), by = "station_id"){
  
  discharge <- db_read_discharge(con, station = station)
  plot_discharge.data.frame(discharge, by = by)
  
}
