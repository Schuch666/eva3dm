#' Calculate daily mean, min or max
#'
#' @description function to calculate Ozone Maximum Daily 8-hr Average or 8-hr moving Average for a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (default is date) in POSIXct
#' @param var name of the columns to be calculated
#' @param stat function of the statistics to calculate (default is mean)
#' @param verbose display additional information
#' @return data.frame
#'
#' @importFrom stats as.formula
#' @export
#'
daily <- function(data, time = 'date', var, stat = mean, verbose = TRUE) {

  if(verbose)
    cat('calculating daily statistcis ... \n')

  data         <- as.data.frame(data)
  data[[time]] <- as.POSIXct(as.Date(data[[time]]),tz = 'UTC')

  if(missing(var)){
    VARS  = names(data)[!names(data) %in% time]
  }
  var = VARS[1]
  output <- aggregate(as.formula(paste0(var,'~',time)),
                      data = data, FUN = stat, na.rm = TRUE)
  for(var in VARS[-1]){
    temp   <- aggregate(as.formula(paste0(var,'~',time)),
                        data = data, FUN = stat, na.rm = TRUE)
    output <- cbind(output,temp[,2])
  }
  names(output) <- names(data)
  return(output)
}
