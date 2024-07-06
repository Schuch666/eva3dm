#' Maximum Daily 8-hr Average
#'
#' @description function to calculate Ozone Maximum Daily 8-hr Average or 8-hr moving Average for a data.frame
#'
#' @param data data.frame with time column and variable columns to be processed
#' @param time name of the time column (defoult is date)
#' @param var name of the columns to be calculated
#' @param verbose display additional information
#' @return data.frame
#' @export
#'
mda8 <- function(data, time = 'date', var, verbose = TRUE) {
  if(verbose)
    cat('calculating MDA8 ... \n')

  data[[time]] <- as.POSIXct(data[[time]])

  if(missing(var)){
    VARS  = names(data)[!names(data) %in% time]
  }
  var           <- VARS[1]
  data$roll_avg <- NA
  for (i in seq_along(data[[var]])) {
    start_time       <- data[[time]][i]
    end_time         <- start_time + 8*3600
    within_8hr       <- data[[time]] >= start_time & data[[time]] < end_time
    data$roll_avg[i] <- mean(data[[var]][within_8hr], na.rm = TRUE)
  }
  data$date          <- as.Date(data[[time]])
  output             <- aggregate(roll_avg ~ date, data = data, FUN = max, na.rm = TRUE)

  for(var in VARS[-1]){
    data$roll_avg <- NA
    for (i in seq_along(data[[var]])) {
      start_time       <- data[[time]][i]
      end_time         <- start_time + 8*3600
      within_8hr       <- data[[time]] >= start_time & data[[time]] < end_time
      data$roll_avg[i] <- mean(data[[var]][within_8hr], na.rm = TRUE)
    }
    daily_max          <- aggregate(roll_avg ~ date, data = data, FUN = max, na.rm = TRUE)
    output             <- cbind(output,var = daily_max[,2])
  }
  names(output) <- c(time,VARS)
  return(output)
}
