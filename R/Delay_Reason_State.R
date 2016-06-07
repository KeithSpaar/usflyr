#' Calculate the area of an ellipse.
#'
#' @param Origin Origin state abbreviation
#' @param Destination Destination state abbreviation
#' @return The most common cause for delay in flights between the given states
#' @examples
#' Delay_Reason_State("NJ")
#' Delay_Reason_State("NJ", "CO")
#' @export
load("data/FlightData.RData")
flights <- flightsSub
rm(flightsSub)


Delay_Reason_State <- function(Origin, Destination = NULL)
  if(is.null(Destination)){
    CARR <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & !is.null(flights$CARRIER_DELAY) & flights$CARRIER_DELAY > 0 ),])
    WEAT <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & !is.null(flights$WEATHER_DELAY) & flights$WEATHER_DELAY > 0 ),])
    NAS <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & !is.null(flights$NAS_DELAY) & flights$NAS_DELAY > 0 ),])
    SEC <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & !is.null(flights$SECURITY_DELAY) & flights$SECURITY_DELAY > 0 ),])
    LATEAIR <- nrow(flights[which(flights$ORIGIN == Origin & !is.null(flights$LATE_AIRCRAFT_DELAY) & flights$LATE_AIRCRAFT_DELAY > 0 ),])
    m <- max(c(CARR, WEAT, NAS, SEC, LATEAIR))
    if(CARR == m){
      return("Carrier Delay")
    } else if(WEAT == m) {
      return("Weather Delay")
    } else if(NAS == m) {
      return("National Aviation System Delay")
    } else if(SEC == m) {
      return("Security Delay")
    } else if (LATEAIR == m)
      return("Late Aircraft Arrival Delay")
  } else {
    CARR <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & flights$DEST_STATE_ABR == Destination & !is.null(flights$CARRIER_DELAY) & flights$CARRIER_DELAY > 0 ),])
    WEAT <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & flights$DEST_STATE_ABR == Destination & !is.null(flights$WEATHER_DELAY) & flights$WEATHER_DELAY > 0 ),])
    NAS <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & flights$DEST_STATE_ABR == Destination & !is.null(flights$NAS_DELAY) & flights$NAS_DELAY > 0 ),])
    SEC <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & flights$DEST_STATE_ABR == Destination & !is.null(flights$SECURITY_DELAY) & flights$SECURITY_DELAY > 0 ),])
    LATEAIR <- nrow(flights[which(flights$ORIGIN_STATE_ABR == Origin & flights$DEST_STATE_ABR == Destination & !is.null(flights$LATE_AIRCRAFT_DELAY) & flights$LATE_AIRCRAFT_DELAY > 0 ),])
    m <- max(c(CARR, WEAT, NAS, SEC, LATEAIR))
    if(CARR == m){
      return("Carrier Delay")
    } else if(WEAT == m) {
      return("Weather Delay")
    } else if(NAS == m) {
      return("National Aviation System Delay")
    } else if(SEC == m) {
      return("Security Delay")
    } else if (LATEAIR == m)
      return("Late Aircraft Arrival Delay")
  }
