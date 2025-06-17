#' Calculate hourly mean, min or max
#'
#' @description function to calculate Ozone Maximum Daily 8-hr Average or 8-hr moving Average for a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (default is date) in POSIXct
#' @param stat function of the statistics to calculate (default is mean)
#' @param min_offset minutes of observation from previous hour (default is 30)
#' @param numerical TRUE (default) includes only numerical columns
#' @param verbose display additional information
#' @return data.frame including only numerical columns
#'
#' @return data.frame with time and the hourly mean, min or max
#'
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' # in case there is connection issue
#' load_data <- function(cond) {
#'   message(paste("conection issue, loading pre-downloaded data"))
#'   DATA <- readRDS(paste0(system.file("extdata",package="eva3dm"),
#'                          "/riem_OAHR_jan_2012.Rds"))
#'   return(DATA)
#' }
#'
#' sites <- c("OAHR")
#' for(site in sites){
#'   cat('Trying to download METAR from:',site,'...\n')
#'   DATA <- tryCatch(riem::riem_measures(station    = sites,
#'                                        date_start = "2012-01-01",
#'                                        date_end   = "2012-02-01"),
#'                    error = load_data)
#' }
#' data_hourly_mean <- hourly(DATA,time = 'valid')
#' data_hourly_min  <- hourly(DATA[1:7],time = 'valid',stat = min)
#' data_hourly_max  <- hourly(DATA[1:7],time = 'valid',stat = max)
#'
hourly <- function(data, time = 'date', stat = mean, min_offset = 30,
                   numerical = TRUE, verbose = TRUE){

  if(verbose) cat('processing hourly data ... \n')

  data               <- as.data.frame(data)
  data[[time]]       <- as.POSIXct(data[[time]],tz = 'UTC')
  data[[time]]       <- data[[time]] + min_offset*60
  data$hour_internal <- format(data[[time]], "%Y-%m-%d %H:00:00")
  data$hour_internal <- as.POSIXct(data$hour_internal,tz = 'UTC')
  if(numerical){
    numeric_data     <- data[, sapply(data, is.numeric)]
  }else{
    numeric_data     <- data # nocov
  }
  hourly_average     <- aggregate(numeric_data,
                                  by = list(data$hour_internal),
                                  FUN = stat,
                                  na.rm = TRUE)
  colnames(hourly_average)[1] <- time
  for(i in 1:ncol(hourly_average)){
    hourly_average[is.nan(hourly_average[,i])     ,i] = NA
    hourly_average[is.infinite(hourly_average[,i]),i] = NA
  }
  class(hourly_average) <- c("data.frame","hourly")
  return(hourly_average)
}
