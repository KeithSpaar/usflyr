#' Calculate the average airtime of all the filghts airport-wise
#'
#' @param Origin Origin Airport
#' @param Destination Destination Airport
#' @return Average of the total airtime for the flight data
#' @examples
#' Avg_time_airport("JFK", "LAX")
#' @export
#'

load("data/FlightData.RData")
flights <- flightsSub
rm(flightsSub)

Avg_time_airport <- function(origin , Destination) {
  return (mean(flights[which(flights$ORIGIN == origin & flights$DEST == Destination
                             & flights$MONTH %in% month), ]$AIR_TIME, na.rm = T)/60)
}
#' Calculate the average airtime of all the filghts airport-wise
#'
#' @param Origin Origin Airport
#' @param Destination Destination Airport
#' @return Average of the total airtime for the flight data
#' @examples
#' Avg_time_airport("JFK", "LAX")
#' @export
#'

Avg_time_airport <- function(origin , Destination) {
  return (mean(flights[which(flights$ORIGIN == origin & flights$DEST == Destination
                             & flights$MONTH %in% month), ]$AIR_TIME, na.rm = T)/60)
}
