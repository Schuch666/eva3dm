#' conversion of model precipitation to hourly precipitation
#'
#' @description function that convert model accumulated precipitation to hourly precipitation.
#'
#' @param rainc data.frame or SpatRaster with RAINC variable
#' @param rainnc data.frame or SpatRaster with RAINNC variable
#' @param verbose set TRUE to display additional information
#'
#' @export
#'
#' @examples
#' times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
#'              as.POSIXct('2024-01-01 04:00:00',tz = 'UTC'),
#'              by = 'hour')
#' RNC   <- data.frame(date = times, aa = c(0.149,0.149,0.149,0.149,0.149))
#' RNNC  <- data.frame(date = times, aa = c(0.919,1.0,1.1,1.1,2.919))
#' rain(rainc = RNC, rainnc = RNNC)

rain <- function(rainc,rainnc, verbose = TRUE){

  if(class(rainc) %in% 'data.frame'){
    deacumulate <- function(x){
      rain    <- x
      for(i in 1:(length(x)-1)){
        rain[i+1] = x[i+1] - x[i]
      }
      rain[1] <- NA
      rain[ rain < 0 ] = NA  # to remove negative values
      return(rain)
    }

    RAIN <- rainc

    for(i in colnames(rainc)[-1]){
      RAIN[,i] = deacumulate(rainc[,i] + rainnc[,i])
    }
    return(RAIN)
  }

  if(class(rainc) %in% 'SpatRaster'){ # nocov start
    RAIN <- rainc + rainnc
    diff <- RAIN[[1]]
    diff[] = NA              # first layer is NA
    for(i in 2:nlyr(RAIN)){
      if(verbose)
        cat('processing layer',i,'of',nlyr(RAIN),'...\n')
      DIFF <- RAIN[[i]] - RAIN[[i-1]]
      if(as.numeric(global(DIFF,'sum')) < 0){
        if(verbose)
          cat('difference is negative, changing to NA\n')
        DIFF[] = NA
      }
      add(diff) <- DIFF
    }
    return(diff)
  } # nocov end

}
