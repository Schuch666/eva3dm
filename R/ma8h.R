#' Calculate 8-hour moving average
#'
#' @description function to calculate Ozone 8-hour moving average for a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (defoult is date) in POSIXct
#' @param var name of the columns to be calculated
#' @param verbose display additional information
#' @return data.frame
#' @export
#'
ma8h <- function(data, time = 'date', var, verbose = TRUE) {

  if(verbose)
    cat('calculating 8-hour moving avarage ... \n')

  data[[time]] <- as.POSIXct(data[[time]], tz = 'UTC')

  if(missing(var)){
    VARS  = names(data)[!names(data) %in% time]
  }
  var           <- VARS[1]
  data$roll_avg <- NA
  for (i in 1:(length(data[[var]])-5)) {
    start_time         <- data[[time]][i]
    end_time           <- start_time + 8*3600
    within_8hr         <- data[[time]] >= start_time & data[[time]] < end_time
    data$roll_avg[i+3] <- mean(data[[var]][within_8hr], na.rm = TRUE)
  }
  output             <- data[,c(time,'roll_avg')]
  for(var in VARS[-1]){
    data$roll_avg <- NA
    for (i in 1:(length(data[[var]])-5)) {
      start_time         <- data[[time]][i]
      end_time           <- start_time + 8*3600
      within_8hr         <- data[[time]] >= start_time & data[[time]] < end_time
      data$roll_avg[i+3] <- mean(data[[var]][within_8hr], na.rm = TRUE)
    }
    output             <- cbind(output,new_var = data$roll_avg)
  }
  names(output) <- c(time,VARS)
  return(output)
}
