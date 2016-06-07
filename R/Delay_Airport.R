#' Calculate the delay time by airport
#'
#' @param Origin State they are leaving from.
#' @param Destination State they are going to.
#' @param Month Month of Departure
#' @return Estimated delay time based one the state the user is going in minutes
#' @examples
#' Delay_Airport("NY", "MI")
#' Delay_Airport("CO")
#' @export

load("data/FlightData.RData")
flights <- flightsSub
rm(flightsSub)
Delay_Airport <- function(origin, destination = NULL, month = c(1:12)) {
   if(is.null(destination)){
     mean(flights[which(flights$ORIGIN == origin & flights$MONTH %in% month),]$DEP_DELAY, na.rm = T)
   }
  else{
     mean(flights[which(flights$ORIGIN == origin & flights$DEST == destination & flights$MONTH %in% month),]$DEP_DELAY, na.rm = T)
   }
}
