#' Selection from data.frames with time-series
#'
#' @description Utility function to select periods from a data.frame. This function is inspired by openair::selectByDate.
#'
#' @param data data.frame with model or observation data
#' @param year numeric vector for selection
#' @param month numeric vector (1-12) for selection, can be abbreviated to 3 or more letters
#' @param day numeric vector (1-31) for selection, weekdays can be abbreviated to 3 or more letters, or weekday/weekend
#' @param hour numeric vector (0-23) for selection
#' @param minutes numeric vector (0-60) for selection
#' @param seconds numeric vector (0-60) for selection
#' @param start POSIXct or character (YYYY-MM-DD) with the initial date of selection
#' @param end POSIXct or character (YYYY-MM-DD) with the initial date of selection
#' @param range pair of start/end or a data.frame with time (default is "date")
#' @param time name of the column for time (default is "date")
#'
#' @return data.frame
#'
#' @seealso See \code{\link[eva3dm]{\%IN\%}} for selection based on position and model domains.
#'
#' @export
#'
#' @examples
#' model <- readRDS(paste0(system.file("extdata",package="eva3dm"),
#'                         "/model.Rds"))
#' summary(model)
#' summary(select(data = model, start = '2012-01-09'))
#' summary(select(data = model, start = '2012-01-05', end  = '2012-01-09'))
#' summary(select(data = model, day  = 6))
#' summary(select(data = model, hour = 12))
#' summary(select(data = model, day = 6, hour = 12))
#' summary(select(data = model, day  = 'weekday'))
#' summary(select(data = model, day  = 'weekend'))
#' summary(select(data = model, day  = 'tue'))
#' summary(select(data = model, day  = 'jan'))

select <- function (data,
                    year, month, day, hour, minutes, seconds,
                    start, end, range,
                    time = "date")
{

  dd <- function(x) as.numeric(format(x,"%d"))
  mm <- function(x) as.numeric(format(x,"%m"))
  yy <- function(x) as.numeric(format(x,"%Y"))
  hh <- function(x) as.numeric(format(x,"%H"))
  mi <- function(x) as.numeric(format(x,"%M"))
  ss <- function(x) as.numeric(format(x,"%S"))

  if(!missing(range)){
    if(is.data.frame(range)){
      range <- base::range(range[,time], na.rm = TRUE)
      start <- range[1]
      end   <- range[2]
    }else{
      start <- range[1] # nocov
      end   <- range[2] # nocov
    }
  }

  if (!missing(start)) {
    if(is.character(start))
      start <- as.POSIXct(start)
    data <- data[data[[time]] >= start, ]
  }
  if (!missing(end)) {
    if(is.character(end))
      end  <- as.POSIXct(end)
    data <- data[data[[time]] <= end, ]
  }
  if (!missing(year)) {
    data <- data[which(yy(data[,time]) %in% year), ]
  }
  if (!missing(month)) {
    if (is.numeric(month)) {
      if (any(month < 1 | month > 12)) {
        stop("Month must be between 1 to 12.") # nocov
      }
      data <- data[which(mm(data[,time]) %in% month),]
    }
    else {
      data <- data[substr(tolower(format(data[[time]], "%B")), 1, 3) %in% substr(tolower(month), 1, 3), ]
    }
  }
  if (!missing(hour)) {
    if (any(hour < 0 | hour > 23))
      stop("Hour must be between 0 to 23.") # nocov
    data <- data[which(hh(data[,time]) %in% hour),]
  }
  if (!missing(minutes)) {
    if (any(minutes < 0 | minutes > 60))
      stop("Minutes must be between 0 to 60.") # nocov
    data <- data[which(mi(data[,time]) %in% minutes),]
  }
  if (!missing(seconds)) {
    if (any(seconds < 0 | seconds > 60))
      stop("Seconds must be between 0 to 60.") # nocov
    data <- data[which(ss(data[,time]) %in% seconds),]
  }
  if (!missing(day)) {
    days <- day
    if (is.numeric(day)) {
      if (any(day < 1 | day > 31)) {
        stop("Day must be between 1 to 31.") # nocov
      }
      data <- data[which(dd(data[,time]) %in% day),]
    }
    else {
      weekday.names <- format(ISOdate(2000, 1, 3:9), "%A")
      if (day[1] == "weekday") {
        days <- weekday.names[1:5]
      }
      if (day[1] == "weekend") {
        days <- weekday.names[6:7]
      }
      data <- data[substr(tolower(format(data[[time]], "%A")), 1, 3) %in% substr(tolower(days), 1, 3), ]
    }
  }
  return(data)
}
