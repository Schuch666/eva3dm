#' Model statistical evaluation
#'
#' @description Statistical (or categorical) evaluation from 2 data.frames. The input data.frames (model and observation)
#' must contain a time column (containing POSIXlt). The function perform pre-conditioning of the data,
#' data pairing (using the time column) of the observations and model and calculate the metrics for statistical or
#' categorical evaluation.
#'
#' @param mo data.frame with model data
#' @param ob data.frame with observation data
#' @param rname row name of the output (default is site argument)
#' @param table data.frame to append the results
#' @param site name of the station or "ALL" (default) or "complete", see notes
#' @param wd default is FALSE, see notes
#' @param fair model data.frame (or list of names) to perform a fair comparison, see notes
#' @param cutoff minimum (optionally the maximum) valid value for observation
#' @param cutoff_NME minimum (optionally the maximum) valid value for observation for NME
#' @param no_tz ignore tz from input (force GMT)
#' @param nobs minimum number of valid observations, default is 8
#' @param eval_function evaluation function (default is stat)
#' @param select_time select the observation (ob) using time from model (mo) data.frame
#' @param time name of the time column (containing time in POSIXct)
#' @param remove_ch remove special characters on column names
#' @param verbose display additional information
#' @param ... arguments to be passing to stats and plot
#'
#' @return data.frame with statistical metric
#'
#' @note Fair argument can be a data.frame or a character string to be used for the analysis, alternatively the function %IN% can be used instead, for example model_d01 %IN% model_d02.
#'
#' @note If wd = TRUE, used for wind direction, a rotation of 360 (or -360) is applied to minimize the wind direction difference.
#'
#' @note If site == 'ALL' (default) all the columns from observations are combined in one column
#' (same for observation) and all the columns are evaluated together.
#'
#' @note If site == 'complete' a internal loop, calls recursively eva() to evaluate all sites in the first argument (model) and using all sites (see "ALL").
#'
#' @note Special thanks to Kiarash and Libo to help to test the wind direction option.
#'
#' @seealso \code{\link{stat}} for additional information about the statistical metrics and \code{\link{cate}} for categorical metrics, and check the example with the custom evaluation function (inclusion of p.value from stats::cor.test()).
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
#' table <- eva(mo = model, ob = obs, site = "VVIbes")
#' print(table)
#'
#' # if the site are not in the input data frame a message is displayed
#' # and the function return an empty row
#' table <- eva(mo = model, ob = obs, site = "Ibirapuera")
#' print(table)
#'
#' # calculating statistical with a few observed values
#' table <- eva(mo = model, ob = obs, site = "Americana")
#' print(table)
#'
#' # calculating categorical (using 2 for threshold) with a few observed values
#' table <- eva(mo = model, ob = obs, site = "Americana",
#'              eval_function = cate, threshold = 2)
#' print(table)
#'
#' # calculating categorical (using 10 for threshold) with a few observed values
#' table <- eva(mo = model, ob = obs, site = "Americana",
#'              eval_function = cate, threshold = 10)
#' print(table)
#'
#'# customizing the evaluation function: inclusion of p.value from stats::cor.test()
#' stat_p <- function(x, y, ...){
#'   table         <- eva3dm::stat(x, y, ...)
#'   cor.result    <- stats::cor.test(x, y, ... )
#'   table$p.value <- cor.result$p.value
#'   table         <- table[,c(1:4,12,5:11)]
#'   return(table)
#' }
#'
#' table <- eva(mo = model, ob = obs, site = "Americana",eval_function = stat_p)
#' print(table)
#'
eva <- function(mo, ob, rname = site, table = NULL,
                site = 'ALL', wd = FALSE, fair = NULL,
                cutoff = NA, cutoff_NME = NA, no_tz = FALSE,
                nobs = 8, eval_function = stat, select_time,
                time = 'date', remove_ch = FALSE,
                verbose = TRUE, ...){

  if(!is.data.frame(mo))
    stop('mo must be a data.frame') # nocov
  if(!is.data.frame(ob))
    stop('ob must be a data.frame') # nocov
  if(!time %in% names(mo))
    stop('mo must have a column named date with times (POSIXct)') # nocov
  if(!time %in% names(ob))
    stop('ob must have a column named date with times (POSIXct)') # nocov

  if(length(class(ob)) > 1) # to keep only data.frame class and methods
    ob <- as.data.frame(ob) # nocov
  if(length(class(mo)) > 1)
    mo <- as.data.frame(mo) # nocov

  if(missing(select_time)){
    select_time = nrow(ob) >= nrow(mo)
  }

  if(select_time){
    ob <- select(data = ob, range = mo, time = time)
  }

  if(remove_ch){
    names(mo) <- iconv(names(mo), from = 'UTF-8', to = 'ASCII//TRANSLIT') # nocov
    names(ob) <- iconv(names(ob), from = 'UTF-8', to = 'ASCII//TRANSLIT') # nocov
  }

  if(site == 'complete'){
    sites  <- names(model)[!names(model) %in% time]                    # nocov
    RESULT <- data.frame()                                             # nocov
    for(s in sites){                                                   # nocov
      RESULT <- eva(mo = mo, ob = ob,table = RESULT, site = s, ... )   # nocov
    }                                                                  # nocov
    RESULT <- eva(mo = mo, ob = ob,table = RESULT, site = 'ALL', ... ) # nocov
    return(RESULT)                                                     # nocov
  }

  if(site == "ALL"){
    if(verbose)  cat('combining all sites...\n')
    site_obs     <- names(ob)[-1]
    if(!is.null(fair)){
      if(verbose)  cat('considering a fair comparison for other domain...\n')
      if('data.frame' %in% class(fair)){
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
      if(verbose) cat(common_sites[i],' ')
      new_mo            <- data.frame(date = mo[,time],
                                      ALL  = mo[[common_sites[i]]])
      new_mo$date       <- new_mo$date + (i- 1) * a_number
      combination_model <- rbind(combination_model, new_mo)

      new_ob            <- data.frame(date = ob[,time],
                                      ALL  = ob[[common_sites[i]]])
      new_ob$date       <- new_ob$date + (i - 1) * a_number
      combination_obs   <- rbind(combination_obs, new_ob)
    }
    if(verbose) cat('...\n')
    mo <- combination_model
    ob <- combination_obs
  }else{
    if(!site %in% names(ob)){
      if(verbose) cat(site,'not found in observation input\n')
      RESULT <- eval_function((1:19)/10,(1:19)/10, ...)
      RESULT[,] = NA
      RESULT$n  = 0
      row.names(RESULT) <- rname
      return(rbind(table,RESULT))
    }
    if(!site %in% names(mo)){
      if(verbose) cat(site,'not found in model input\n')
      RESULT <- eval_function((1:19)/10,(1:19)/10, ...)
      RESULT[,] = NA
      RESULT$n  = 0
      row.names(RESULT) <- rname
      return(rbind(table,RESULT))
    }
  }

  model        <- mo[,c(time,site)]
  names(model) <- c(time,"model")
  obser        <- ob[,c(time,site)]
  names(obser) <- c(time,"obser")
  if(no_tz){
    f <- function(x,tz="GMT") return(as.POSIXct(as.numeric(x), origin="1970-01-01", tz=tz))
    model$date <- f(model$date)
    obser$date <- f(model$date)
  }
  DATA  <- merge(model, obser, by = time, all.x = TRUE)
  A     <- DATA$model
  B     <- DATA$obser

  to_run = TRUE
  if(suppressWarnings( max(A,na.rm = T) ) == suppressWarnings( min(A,na.rm = TRUE)) ){
    if(verbose) cat(site,'contains only zeros (or constant values) and NA values for model\n')
    to_run = FALSE
  }
  if(suppressWarnings(  max(B,na.rm = T) ) == suppressWarnings( min(B,na.rm = TRUE)) ){
    if(verbose) cat(site,'contains only zeros (or constant values) and NA values for observations\n')
    to_run = FALSE
  }

  if(length(B[!is.na(B)]) > nobs & to_run){
    if(verbose)
      cat(site,'has',length(B[!is.na(B)]),'valid observations\n')
    RESULT <- eval_function(A,B, cutoff=cutoff,cutoff_NME=cutoff_NME, wd = wd, nobs = nobs, ...)
    row.names(RESULT) <- rname
  }else{
    if(verbose & to_run) cat(site,'has only',length(B[!is.na(B)]),'valid observations (lesser than',nobs,'obs)\n')
    RESULT <- eval_function((1:19)/10,(1:19)/10, ...)
    RESULT[,] = NA
    RESULT$n  = 0
    row.names(RESULT) <- rname
  }
  return(rbind(table,RESULT))
}

#' Returns the common columns
#' @description results of 'd01 in d02' style syntax
#'
#' @param x data.frame
#' @param y data.frame or character string
#'
#' @return data.frame with common columns or a cropped SpatRaster
#'
#' @note A message is always displayed to keep easy to track and debug issues (with the results and the evaluation process).
#'
#' @note Can be used to crop rast objects, such as arguments of sat() function
#'
#' @seealso See \code{\link[eva3dm]{select}} for selection based on time.
#'
#' @export
#' @import terra
#'
#' @examples
#' times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
#'              as.POSIXct('2024-01-02',tz = 'UTC'),
#'              by = 'hour')
#' randon_stuff <- rnorm(25,10)
#'
#' observation <- data.frame(date   = times,
#'                           site_1 = randon_stuff,
#'                           site_3 = randon_stuff,
#'                           site_4 = randon_stuff,
#'                           site_5 = randon_stuff,
#'                           site_6 = randon_stuff,
#'                           site_7 = randon_stuff)
#'
#' model_d01 <- data.frame(date  = times,
#'                         site_1=randon_stuff+1,
#'                         site_2=randon_stuff+2,
#'                         site_3=randon_stuff+3,
#'                         site_4=randon_stuff+4)
#'
#' model_d02 <- data.frame(date  = times,
#'                         site_1=randon_stuff-1,
#'                         site_3=randon_stuff-3)
#'
#' # multiline
#' model_d01_in_d02 <- model_d01 %IN% model_d02
#' eva(mo = model_d01_in_d02, ob = observation, rname = 'd01 in d02')
#'
#' # or single line
#' eva(mo = model_d01 %IN% model_d02, ob = observation, rname = 'd01 in d02')
#' # or
#' eva(mo = model_d01, ob = observation %IN% model_d02, rname = 'd01 in d02')
#'
#'
`%IN%` <- function(x, y){

  if('SpatRaster' %in% class(x) & 'SpatRaster' %in% class(y)){
    cat('croping',deparse(substitute(x)),'with',deparse(substitute(y)),'\n')
    e <- ext(terra::project(y,crs(x,proj = T)))
    return(crop(x,e))
  }

  cat('using',deparse(substitute(x)),'in',deparse(substitute(y)),'\n')

  if(!is.data.frame(x))
    stop('x must be a data.frame') # nocov
  if(!is.data.frame(y) & !is.character(y))
    stop('y must be a data.frame or character') # nocov

  if(is.data.frame(y)){
    x <- x[,names(x) %in% names(y)]
  }
  if(is.character(y)){
    x <- x[,names(x) %in% y]
  }
  return(x)
}
