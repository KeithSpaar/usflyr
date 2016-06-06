#' Calculate the average airtime of all the filghts state-wise
#'
#' @param Origin Origin Airport
#' @param Destination Destination Airport
#' @return Average of the total airtime for the flight data
#' @examples
#' Avg_time_state("CA", "CO")
#' @export
#'
#'
load("data/FlightData.RData")
flights <- flightsSub
rm(flightsSub)

Avg_time_state <- function(origin, Destination, months = c(1:12)) {
  return (mean(flights[which(flights$ORIGIN_STATE_ABR==origin
                             & flights$DEST_STATE_ABR==Destination
                             & flights$MONTH %in% month),]$AIR_TIME,na.rm = T))
}
