#' Calculate yearly mean, min or max
#'
#' @description function to calculate yearly mean, min or max of a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (default is date) in POSIXct
#' @param stat function of the statistics to calculate (default is mean)
#' @param min_offset minutes of observation from previous year (default is 0)
#' @param hour_offset hours of observation from previous year (default is 0)
#' @param days_offset day of observation from previous year (default is 0)
#' @param month_offset months of observation from previous year (default is 0)
#' @param numerical TRUE (default) include only numerical columns
#' @param verbose display additional information
#'
#' @return data.frame with time and the yearly mean, min or max
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
#'              as.POSIXct('2025-12-31',tz = 'UTC'),
#'              by = 'day')
#'
#' DATA <- data.frame(date = times,
#'                    var1 = rnorm(n = length(times), mean = 1,sd = 1),
#'                    var2 = rnorm(n = length(times), mean = 2,sd = 0.5),
#'                    var3 = rnorm(n = length(times), mean = 3,sd = 0.25))
#'
#' data_year_mean <- yearly(DATA)
#' data_year_min  <- yearly(DATA,stat = min)
#' data_year_max  <- yearly(DATA,stat = max)
#'
yearly <- function(data, time = 'date', stat = mean,
                  min_offset = 0, hour_offset = 0,
                  days_offset = 0, month_offset = 0,
                  numerical = TRUE, verbose = TRUE) {

  if(verbose) cat('processing yearly statistcis ... \n')

  data                <- as.data.frame(data)
  data[[time]]        <- as.POSIXct(as.Date(data[[time]]),tz = 'UTC')
  data[[time]]        <- data[[time]] + 24*hour_offset*60*60 + hour_offset*60*60 + min_offset*60
  data$daily_internal <- format(data[[time]], "%Y-06-15 %00:00:00")
  data$daily_internal <- as.POSIXct(data$daily_internal,tz = 'UTC')
  if(numerical){
    numeric_data     <- data[, sapply(data, is.numeric)]
  }else{
    numeric_data     <- data # nocov
  }
  daily_average     <- aggregate(numeric_data,
                                  by = list(data$daily_internal),
                                  FUN = stat,
                                  na.rm = TRUE)
  colnames(daily_average)[1] <- time
  for(i in 1:ncol(daily_average)){
    daily_average[is.nan(daily_average[,i])     ,i] = NA
    daily_average[is.infinite(daily_average[,i]),i] = NA
  }
  class(daily_average) <- c("data.frame","yearly")
  return(daily_average)
}
