#' Calculate daily mean, min or max
#'
#' @description function to calculate daily mean, min or max of a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (default is date) in POSIXct
#' @param stat function of the statistics to calculate (default is mean)
#' @param min_offset minutes of observation from previous hour (default is 0)
#' @param hour_offset hours of observation from previous day (default is 0)
#' @param numerical TRUE (default) include only numerical columns
#' @param verbose display additional information
#'
#' @return data.frame with time and the daily mean, min or max
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' # in case there is connection issue
#' load_data <- function(cond) {
#'   message(paste("conection issue, loading pre-downloaded data"))
#'   DATA <- readRDS(paste0(system.file("extdata",package="eva3dm"),
#'                          "/riem_OAKB_jan_2012.Rds"))
#'   return(DATA)
#' }
#'
#' sites <- c("OAKB")
#' for(site in sites){
#'   cat('Trying to download METAR from:',site,'...\n')
#'   DATA <- tryCatch(riem::riem_measures(station    = sites,
#'                                        date_start = "2012-01-01",
#'                                        date_end   = "2012-02-01"),
#'                    error = load_data)
#' }
#' data_daily_mean <- daily(DATA,time = 'valid')
#' data_daily_min  <- daily(DATA[1:7],time = 'valid',stat = min)
#' data_daily_max  <- daily(DATA[1:7],time = 'valid',stat = max)
#'
daily <- function(data, time = 'date', stat = mean, min_offset = 0,
                  hour_offset = 0, numerical = TRUE, verbose = TRUE) {

  if(verbose) cat('processing daily statistcis ... \n')

  data                <- as.data.frame(data)
  data[[time]]        <- as.POSIXct(as.Date(data[[time]]),tz = 'UTC')
  data[[time]]        <- data[[time]] + hour_offset*60*60 + min_offset*60
  data$daily_internal <- format(data[[time]], "%Y-%m-%d %00:00:00")
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
  class(daily_average) <- c("data.frame","daily")
  return(daily_average)
}
