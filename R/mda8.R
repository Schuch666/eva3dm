#' Maximum Daily 8-hr Average
#'
#' @description function to calculate Ozone Maximum Daily 8-hr Average or 8-hr moving Average for a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (defoult is date) in POSIXct
#' @param var name of the columns to be calculated
#' @param verbose display additional information
#' @return data.frame
#'
#' @importFrom stats as.formula
#' @export
#'
mda8 <- function(data, time = 'date', var, verbose = TRUE) {

  ma8h <- ma8h(data = data, time = time, var = var, verbose = verbose)

  if(verbose)
    cat('calculating daily maximum ... \n')
  output <- daily(data = data, time = time, var = var, verbose = F, stat = max)

  return(output)
}
