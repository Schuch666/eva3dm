#' Convert absolute humidity to relative humidity
#'
#' @description function to convert absolute humidity to relative humidity.
#'
#' @param q vector (or data.frame) of relative humidity (in g/Kg)
#' @param t vector (or data.frame) of temperature (in Celcius)
#' @param p vector (or data.frame) of pressure (in Pa)
#'
#' @note default values are from standard atmosphere (288.15 K / 101325 Pa)
#' @note if rh and temp arguments are data.frame, both need to have the same number of lines and columns, first column (time column) will be ignored.
#'
#' @export
#'
#' @return values of a data.frame, units are %
#'
#' @examples
#' # for a single value (or same length vectors)
#' q2rh(q = 0.0002038, t = 29.3, p = 100800)
#'
#' # using all data.frames
#' times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
#'              as.POSIXct('2024-01-02',tz = 'UTC'),
#'              by = 'hour')[1:5]
#' q2   <- data.frame(time = times, a = rep(0.0002038,5))
#' temp <- data.frame(time = times, a = rep(     29.3,5))
#' pres <- data.frame(time = times, a = rep(   100800,5))
#' q2rh(q = q2, t = temp, p = pres)
#'
#' # using data.frame for q and t (p is cte.)
#' q2rh(q = q2, t = temp, p = 100000)
#'
#' # using data.frame for q and p (t is cte.)
#' q2rh(q = q2, t = 26, p = pres)
#'
#' # using data.frame only for q (p and t are cte.)
#' q2rh(q = q2, t = 26, p = 100000)
#'
q2rh <- function(q, t = 15, p = 101325){

  if('data.frame' %in% class(q) & 'data.frame' %in% class(t) & 'data.frame' %in% class(p)){
    obs     <- q
    obs[-1] <- NA
    for(j in 2:ncol(obs)){
      obs[,j] <- q2rh(q = q[,j],t = t[,j],p = p[,j])
    }
    return(obs)
  }
  if('data.frame' %in% class(q) & 'data.frame' %in% class(t)){
    obs     <- q
    obs[-1] <- NA
    for(j in 2:ncol(obs)){
      obs[,j] <- q2rh(q = q[,j],t = t[,j],p = p)
    }
    return(obs)
  }
  if('data.frame' %in% class(q) & 'data.frame' %in% class(p)){
    obs     <- q
    obs[-1] <- NA
    for(j in 2:ncol(obs)){
      obs[,j] <- q2rh(q = q[,j],t = t,p = p[,j])
    }
    return(obs)
  }

  if('data.frame' %in% class(q)){
    obs     <- q
    obs[-1] <- NA
    for(j in 2:ncol(obs)){
      obs[,j] <- q2rh(q = q[,j],t = t,p = p)
    }
    return(obs)
  }

  # t  <- t - 273.15
  e  <- q * p / 0.622
  es <- 610.8 * exp(17.3*t /(t + 237.3))
  RH <- 100 * e / es
  RH[RH > 1] = 1.0
  return(100 * RH)
}
