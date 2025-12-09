#' Calculate 8-hour moving average
#'
#' @description function to calculate Ozone 8-hour moving average for a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (default is date) in POSIXct
#' @param var name of the columns to be calculated
#' @param n custom time window (n = 8 for 8-hour average)
#' @param verbose display additional information
#' @param ... parameters passed to hourly
#'
#' @seealso \code{\link{mda8}} for Maximum Daily 8-hour moving average
#'
#' @return data.frame with time and the 8-hour moving average
#'
#' @export
#'
#' @examples
#' model_file <- paste(system.file("extdata", package = "eva3dm"),
#'                                 "/model_o3_ugm3_36km.Rds", sep="")
#' model      <- readRDS(model_file)
#' model_8h   <- ma8h(model)
#' plot(model$date,model$Campinas, pch = 19,
#'      main = expression(O[3]~~'['*mu*g*m^-3*']'))
#' points(model_8h$date,model_8h$Campinas, col = 'blue', pch = 19)
#' legend('topleft',bty = 'n',
#'        pch = 19,
#'        legend = c('hourly','8h-mov average'),
#'        col = c('black','blue'))
ma8h <- function(data, time = 'date', var, n = 8, verbose = TRUE, ...) {

  if(!'hourly' %in% class(data)){
    data <- hourly(data,time = time, verbose = verbose, ...)
  }

  if(verbose)
    cat(paste0('processing ',n,'-hour moving average ... \n'))

  moving_average <- function(x, n = n) {
    if (n > length(x)) {
       stop("Window size 'n' should be less than or equal to the length of the vector 'x'.") # nocov
     }
    result <- numeric(length(x) - n + 1)
    for (i in 1:(length(x) - n + 3)) {
      result[i] <- mean(x[i:(i + n - 1)], na.rm = TRUE)
   }
    result <- c(NA,NA,NA,result,NA,NA)
   return(result)
  }

  if(missing(var)){
    VARS  = names(data)[!names(data) %in% time]
  }
  var           <- VARS[1]
  data$roll_avg <- moving_average(data[[var]], n = 8)
  output        <- data[,c(time,'roll_avg')]
  for(var in VARS[-1]){
    data$roll_avg <- moving_average(data[[var]], n = 8)
    output        <- cbind(output,new_var = data$roll_avg)
  }
  names(output) <- c(time,VARS)
  class(output) <- c("data.frame","maximum daily 8h average")
  return(output)
}
