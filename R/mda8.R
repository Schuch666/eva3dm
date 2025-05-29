#' Maximum Daily 8-hr Average
#'
#' @description function to calculate Ozone Maximum Daily 8-hr Average or 8-hr moving Average for a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (default is date) in POSIXct
#' @param var name of the columns to be calculated
#' @param verbose display additional information
#'
#' @return data.frame with time and the maximum daily 8-hr average
#'
#' @seealso \code{\link{ma8h}} for 8-hour Moving Average
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' model_file <- paste(system.file("extdata", package = "eva3dm"),
#'                     "/model_o3_ugm3_36km.Rds", sep="")
#' model      <- readRDS(model_file)
#' model_mda8 <- mda8(model)
#' model_8h   <- ma8h(model)
#' plot(model$date,model$Campinas, pch = 19,
#'      main = expression(O[3]~~'['*mu*g*m^-3*']'))
#' points(model_8h$date,model_8h$Campinas, col = 'blue', pch = 19)
#' points(model_mda8$date + 17*60*60,model_mda8$Campinas,
#'        col = 'red', pch = 4, cex = 2)
#' legend('topleft',bty = 'n',
#'        pch = c(19,19,4),
#'        legend = c('hourly','8h-mov average','MD8A'),
#'        col = c('black','blue','red'))
mda8 <- function(data, time = 'date', var, verbose = TRUE) {
  if(!'8-hour moving avarage' %in% class(data)){
    ma8h <- ma8h(data = data, time = time, var = var, verbose = verbose)
  }else{
    ma8h <- data # nocov
  }
  if(verbose) cat('processing daily maximum ... \n')
  output <- daily(data = ma8h, time = time, var = var, verbose = FALSE, stat = max)
  class(output) <- c("data.frame","maximum daily 8h average")
  return(output)
}
