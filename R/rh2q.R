#' Convert relative humidity to absolute humidity
#'
#' @description function to convert humidity to absolute humidity using tetens formula, assuming standart atmosphere conditions.
#'
#' @param rh vector of relative humidity (in percentage)
#' @param temp vector of temperature (in celcius)
#'
#' @note default values are from standard atmosphere (288.15 K)
#'
#' @examples
#' rh2q(99)
#'
#' @export
#'

rh2q <- function(rh, temp = 15) {
  temp  = temp + 273.15
  rh    = rh / 100
  qair <- rh * 2.541e6 * exp(-5415.0 / temp) * 18/29
  return(qair)
}
