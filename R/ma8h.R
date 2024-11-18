#' Calculate 8-hour moving average
#'
#' @description function to calculate Ozone 8-hour moving average for a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (default is date) in POSIXct
#' @param var name of the columns to be calculated
#' @param verbose display additional information
#' @param ... parameters passed to hourly
#' @return data.frame
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
ma8h <- function(data, time = 'date', var, verbose = TRUE, ...) {

  if(!'hourly' %in% class(data)){
    data <- hourly(data,time = time, var = var, verbose = verbose, ...)
  }

  if(verbose)
    cat('processing 8-hour moving avarage ... \n')

  moving_average <- function(x, n = 8) {
    if (n > length(x)) {
       stop("Window size 'n' should be less than or equal to the length of the vector 'x'.") # nocov
     }
    result <- numeric(length(x) - n + 1)
    for (i in 1:(length(x) - n + 3)) {
      result[i] <- mean(x[i:(i + n - 1)], na.rm = T)
   }
    result <- c(NA,NA,NA,result,NA,NA)
   return(result)
  }

  if(missing(var)){
    VARS  = names(data)[!names(data) %in% time]
  }
  var           <- VARS[1]
  data$roll_avg <- moving_average(data[[var]], n = 8)
  # data$roll_avg <- terra::roll(data[[var]],8,mean,'around',na.rm=T)
  output        <- data[,c(time,'roll_avg')]
  for(var in VARS[-1]){
    data$roll_avg <- moving_average(data[[var]], n = 8)
    # data$roll_avg <- terra::roll(data[[var]],8,mean,'around',na.rm=T)
    output        <- cbind(output,new_var = data$roll_avg)
  }
  names(output) <- c(time,VARS)
  class(output) <- c("data.frame","8h-hour moving avarage")
  return(output)
}
