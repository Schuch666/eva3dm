#' Get the distance in kilometers between two points
#'
#' @param lat1 Latitude in decimals
#' @param long1 Longitude in decimals
#' @param lat2 Latitude in decimals
#' @param long2 Longitude in decimals
#' @param R Radius of the earth in kmdescription (R=6371)
#'
#' @return A numeric vector with the distance in kilometers.
#'
#' #' source: https://github.com/gustavobio/brclimate/blob/master/R/get_distances.R
#'
#' @export
#'
get_distances <- function(lat1, long1, lat2, long2, R = 6371) {
  lat1   <- lat1 * pi/180
  lat2   <- lat2 * pi/180
  d_lat  <- lat2 - lat1
  d_long <- (long2 - long1) * pi/180
  a      <- sin(d_lat/2)^2 + cos(lat1) * cos(lat2) * sin(d_long/2)^2
  d      <- R * 2 * atan2(sqrt(a), sqrt(1 - a))
  return(d)
}
