#' Returns the most common cause for delay in flights between the given airports
#'
#' @param Origin Origin airport abbreviation
#' @param Destination Destination airport abbreviation
#' @return The most frequent cause of delay
#' @examples
#' Delay_Reason("JFK")
#' Delay_Reason("JFK", "LAX")
#' @export

load("data/FlightData.RData")
flights <- flightsSub
rm(flightsSub)

Delay_Reason<- function(Origin, Destination = NULL)
  if(is.null(Destination)){
    CARR <- nrow(flights[which(flights$ORIGIN == Origin & !is.null(flights$CARRIER_DELAY) & flights$CARRIER_DELAY > 0 ),])
    WEAT <- nrow(flights[which(flights$ORIGIN == Origin & !is.null(flights$WEATHER_DELAY) & flights$WEATHER_DELAY > 0 ),])
    NAS <- nrow(flights[which(flights$ORIGIN == Origin & !is.null(flights$NAS_DELAY) & flights$NAS_DELAY > 0 ),])
    SEC <- nrow(flights[which(flights$ORIGIN == Origin & !is.null(flights$SECURITY_DELAY) & flights$SECURITY_DELAY > 0 ),])
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
    CARR <- nrow(flights[which(flights$ORIGIN == Origin & flights$DEST == Destination & !is.null(flights$CARRIER_DELAY) & flights$CARRIER_DELAY > 0 ),])
    WEAT <- nrow(flights[which(flights$ORIGIN == Origin & flights$DEST == Destination & !is.null(flights$WEATHER_DELAY) & flights$WEATHER_DELAY > 0 ),])
    NAS <- nrow(flights[which(flights$ORIGIN == Origin & flights$DEST == Destination & !is.null(flights$NAS_DELAY) & flights$NAS_DELAY > 0 ),])
    SEC <- nrow(flights[which(flights$ORIGIN == Origin & flights$DEST == Destination & !is.null(flights$SECURITY_DELAY) & flights$SECURITY_DELAY > 0 ),])
    LATEAIR <- nrow(flights[which(flights$ORIGIN == Origin & flights$DEST == Destination & !is.null(flights$LATE_AIRCRAFT_DELAY) & flights$LATE_AIRCRAFT_DELAY > 0 ),])
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
