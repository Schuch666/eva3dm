#' Model statistical evaluation
#'
#' @description Statistical (or categorical) evaluation from 2 data.frames. The input data.frames (model and observation)
#' must contain a "date" column (containing POSIXlt). The function perform some simple case tests and
#' perform the time pairing of observations and model data and can calculate the statistical evaluation or
#' categorical evaluation.
#'
#' @param mo data.frame with model data
#' @param ob data.frame with observation data
#' @param station name of the station or "ALL", see notes
#' @param rname name of the output row (default is station)
#' @param fair model data.frame (or list of names) to perform a fair comparison, see notes
#' @param wd default is FALSE, see notes
#' @param cutoff minimum (optionally the maximum) valid value for observation
#' @param cutoff_NME minimum (optionally the maximum) valid value for observation for NME
#' @param no_tz ignore tz from input (force GMT)
#' @param nobs minimum number of valid observations, default is 8
#' @param verbose display additional information
#' @param eval_function evaluation function (default is stat)
#' @param ... arguments to be passing to stats and plot
#'
#' @note for wind direction a rotation of 360 (or -360) is applied to minimize the wind direction difference.
#'
#' @note If station == 'ALL' (default) all the columns from observations are combined in one column
#' (same for observation) and all the columns are evaluated together.
#'
#' @note Special thanks to Kiarash and Libo to help to test the wind direction option.
#'
#' @seealso \code{\link{stat}} for additional information about the statistical evaluation and \code{\link{cate}} for categorical evaluation.
#'
#' @export
#'
#' @examples
#' model <- readRDS(paste0(system.file("extdata",package="eva3dm"),
#'                         "/model.Rds"))
#' obs   <- readRDS(paste0(system.file("extdata",package="eva3dm"),
#'                         "/obs.Rds"))
#'
#' # if there is no observed data
#' # the function return an empty row
#' table <- eva(mo = model, ob = obs, station = "VVIbes")
#' print(table)
#'
#' # if the station are not in the input data frame a message is displayed
#' # and the function return an empty row
#' table <- eva(mo = model, ob = obs, station = "Ibirapuera")
#' print(table)
#'
#' # calculating statistical with a few observed values
#' table <- eva(mo = model, ob = obs, station = "Americana")
#' print(table)
#'
#' # calculating categorical (using 2 for threshold) with a few observed values
#' table <- eva(mo = model, ob = obs, station = "Americana",
#'              eval_function = cate, threshold = 2)
#' print(table)
#'
#' # calculating categorical (using 2 for threshold) with a few observed values
#' table <- eva(mo = model, ob = obs, station = "Americana",
#'              eval_function = cate, threshold = 10)
#' print(table)
#'
eva <- function(mo, ob, station = 'ALL', wd = FALSE, fair = NULL,
                cutoff = NA, cutoff_NME = NA, no_tz = FALSE,
                nobs = 8, rname = station, eval_function = stat,
                verbose = TRUE, ...){

  if(station == "ALL"){
    cat('combining all stations...\n')
    site_obs     <- names(ob)[-1]
    if(!is.null(fair)){
      cat('considering a fair comparison for other domain...\n')
      if(class(fair) %in% 'data.frame'){
        site_model   <- names(fair)[-1]
      }else{
        site_model   <- fair
      }
    }else{
      site_model   <- names(mo)[-1]
    }
    common_sites   <- site_model[ site_model %in% site_obs ]

    combination_model <- data.frame()
    combination_obs   <- data.frame()
    a_number          <- 666 * 60 * 60 * 24 * 365 + 161 * 60 * 60 * 24
    for(i in seq_along(common_sites)){
      if(verbose)
        cat(common_sites[i],'\n')
      new_mo            <- data.frame(date = mo$date,
                                      ALL  = mo[[common_sites[i]]])
      new_mo$date       <- new_mo$date + (i- 1) * a_number
      combination_model <- rbind(combination_model, new_mo)

      new_ob            <- data.frame(date = ob$date,
                                      ALL  = ob[[common_sites[i]]])
      new_ob$date       <- new_ob$date + (i - 1) * a_number
      combination_obs   <- rbind(combination_obs, new_ob)
    }
    mo <- combination_model
    ob <- combination_obs
  }else{
    if(!station %in% names(ob)){
      cat(station,'not found in observation input\n')
      RESULT <- eval_function((1:199)/100,(1:199)/100, ...)
      RESULT$n = 0
      row.names(RESULT) <- rname
      return(RESULT)
    }
    if(!station %in% names(mo)){
      cat(station,'not found in model input\n')
      RESULT <- eval_function((1:199)/100,(1:199)/100, ...)
      RESULT$n = 0
      row.names(RESULT) <- rname
      return(RESULT)
    }
  }

  model        <- mo[,c("date",station)]
  names(model) <- c("date","model")
  obser        <- ob[,c("date",station)]
  names(obser) <- c("date","obser")
  if(no_tz){
    f <- function(x,tz="GMT") return(as.POSIXct(as.numeric(x), origin="1970-01-01", tz=tz))
    model$date <- f(model$date)
    obser$date <- f(model$date)
  }
  DATA  <- merge(model, obser, by = "date", all.x = TRUE)
  A     <- DATA$model
  B     <- DATA$obser

  to_run = TRUE
  if(suppressWarnings( max(A,na.rm = T) ) == suppressWarnings( min(A,na.rm = T)) ){
    if(verbose)
      cat(station,'contains only zeros (or constant values) and NA values for model\n')
    to_run = FALSE
  }
  if(suppressWarnings(  max(B,na.rm = T) ) == suppressWarnings( min(B,na.rm = T)) ){
    if(verbose)
      cat(station,'contains only zeros (or constant values) and NA values for observations\n')
    to_run = FALSE
  }

  if(length(B[!is.na(B)]) > nobs & to_run){
    if(verbose)
      cat(station,'has',length(B[!is.na(B)]),'valid observations\n')
    RESULT <- eval_function(A,B, cutoff=cutoff,cutoff_NME=cutoff_NME, wd = wd, nobs = nobs, ...)
    row.names(RESULT) <- rname
  }else{
    if(verbose & to_run)
      cat(station,'has only',length(B[!is.na(B)]),'valid observations (lesser than',nobs,'obs)\n')
    RESULT <- eval_function((1:199)/100,(1:199)/100, ...)
    RESULT$n = 0
    row.names(RESULT) <- rname
  }

  if(RESULT$n > 0){
    if(max(B,na.rm = T) == min(B, na.rm = T)){
      if(verbose)
        cat(station,'values are constant, No of observation set to 0\n')
      RESULT$n = 0
    }
  }
  return(RESULT)
}
