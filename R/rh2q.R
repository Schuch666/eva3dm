#' Convert relative humidity to absolute humidity
#'
#' @description function to convert humidity to absolute humidity using Tetens formula, assuming standard atmosphere conditions.
#'
#' @param rh vector (or data.frame) of relative humidity (in percentage)
#' @param temp vector (or data.frame) of temperature (in Celsius)
#'
#' @note default values are from standard atmosphere (288.15 K / 15 C)
#' @note if rh and temp arguments are data.frame, both need to have the same number of lines and columns, first column (time column) will be ignored.
#'
#' @return value of data.frame with time and the absolute humidity, units are g/g
#'
#' @examples
#' # for a singfle value
#' rh2q(rh = 99, temp = 25)
#'
#' # vector of rh values
#' rh2q(rh = c(0,seq(1,100, by = 4)), temp = 25)
#'
#' # vector of values for rh and temp
#' rh2q(rh = c(0,seq(1,100, by = 4)), temp = 10:35)
#'
#' # rh is data.frame and temp is a value
#' times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
#'              as.POSIXct('2024-01-02',tz = 'UTC'),
#'              by = 'hour')
#' rh2q(rh   = data.frame(time = times, a = seq(1,100, by = 4)),temp = 25)
#'
#' # using both rh and temp are data.frames
#' rh2q(rh   = data.frame(time = times, a = seq(1,100, by = 4)),
#'      temp = data.frame(time = times, a = 11:35))
#' @export
#'
rh2q <- function(rh, temp = 15) {
  if('data.frame' %in% class(rh) & 'data.frame' %in% class(temp)){
    obs     <- rh
    obs[-1] <- NA
    for(j in 2:ncol(rh)){
      obs[,j] <- rh2q(rh = rh[,j],temp = temp[,j])
    }
    return(obs)
  }
  if('data.frame' %in% class(rh)){
    obs     <- rh
    obs[-1] <- NA
    for(j in 2:ncol(rh)){
      obs[,j] <- rh2q(rh = rh[,j],temp = temp)
    }
    return(obs)
  }
  temp  = temp + 273.15
  rh    = rh / 100
  qair <- rh * 2.541e6 * exp(-5415.0 / temp) * 18/29
  return(qair)
}
