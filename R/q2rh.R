#' Convert absolute humidity to relative humidity
#'
#' @description function to convert absolute humidity to relative humidity.
#'
#' @param q vector of relative humidity (in Kg/Kg)
#' @param t vector of temperature (in K)
#' @param p pressure (in Pa)
#'
#' @note default values are from standard atmosphere (288.15 K / 101325 Pa)
#'
#' @export
#'
#' @examples
#' q2rh(q = 0.0002038, t = 302.45, p = 100800)
#'

q2rh <- function(q, t = 288.15, p = 101325){
  t  <- t - 273.15
  e  <- q * p / 0.622
  es <- 610.8 * exp(17.3*t /(t + 237.3))
  RH <- 100 * e / es
  RH[RH > 1] = 1.0
  return(100 * RH)
}
