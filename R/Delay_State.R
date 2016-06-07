#' Calculate the delay time by state and by airport
#'
#' @param origin State they are leaving from.
#' @param destination State they are going to.
#' @param Month Month of Departure
#' @return Estimated delay time based one the state the user is going.
#' @examples
#' Delay_State("NY", "MI")
#' Delay_State("CO")
#' @export
load("data/FlightData.RData")
flights <- flightsSub
rm(flightsSub)
Delay_State <- function(origin, destination = NULL, month = c(1:12)){
  if(is.null(destination)){
    mean(flights[which(flights$ORIGIN_STATE_ABR == origin & flights$MONTH %in% month),]$DEP_DELAY, na.rm = T)
  }
  else{
    mean(flights[which(flights$ORIGIN_STATE_ABR == origin & flights$DEST_STATE_ABR == destination & flights$MONTH %in% month),]$DEP_DELAY, na.rm = T)
  }

}
