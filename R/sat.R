#' @title Functions to model evaluation using satellite
#'
#' @description functions to evaluate the spatial performance using satellite
#'
#' @return a data.frame
#'
#' @param mo SpatRaster or raster with model
#' @param ob SpatRaster or raster with observations
#' @param rname passed to stat
#' @param table data.frame to append the results
#' @param n number of points from the boundary removed, default is 5
#' @param min minimum value cutoff
#' @param max maximum value cutoff
#' @param method passed to terra::resample
#' @param eval_function evaluation function (default is stat)
#' @param mask optional SpatVector to mask the results
#' @param skip_interp skip the interpolation step
#' @param verbose set TRUE to display additional information
#' @param ... other arguments passed to stat
#'
#' @note If a YOU DIED error message appears, means you are removing all the valid values using the arguments min or max.
#' @note If cate() is used for eval_function, the argument threshold must be included (see example).
#'
#' @import terra
#'
#' @examples
#' model_no2 <- terra::rast(paste0(system.file("extdata",package="eva3dm"),
#'                               "/camx_no2.Rds"))
#' omi_no2   <- terra::rast(paste0(system.file("extdata",package="eva3dm"),
#'                               "/omi_no2.Rds"))
#'
#' # generate the statistical indexes
#' sat(mo = model_no2,ob = omi_no2,rname = 'NO2_statistical')
#'
#' # generate categorical evaluation using 3.0 as threshold
#' sat(mo = model_no2,ob = omi_no2,rname = 'NO2_categorical',
#'     eval_function = cate, threshold = 3.0)
#'
#' # customizing the evaluation function: inclusion of p.value from stats::cor.test()
#' stat_p <- function(x, y, ...){
#'   table         <- eva3dm::stat(x, y, ...)
#'   cor.result    <- stats::cor.test(x, y, ... )
#'   table$p.value <- cor.result$p.value
#'   table         <- table[,c(1:4,12,5:11)]
#'   return(table)
#' }
#'
#' sat(mo = model_no2,ob = omi_no2,rname = 'NO2_statistical',eval_function = stat_p)
#'
#' @export

sat <- function(mo,ob,rname, table = NULL,
                n = 6, min = NA, max = NA,
                method = 'bilinear', eval_function = stat,
                mask, skip_interp = FALSE, verbose = TRUE, ...){

  if(missing(mo))
    stop('model input is missing!') # nocov

  if(missing(ob))
    stop('observation input is missing!') # nocov

  if(class(mo) %in% c('RasterLayer','RasterBrick')){
    mo <- rast(mo) # nocov
  }
  if(class(ob) %in% c('RasterLayer','RasterBrick')){
    ob <- rast(ob) # nocov
  }

  cut_boundary <- function(x, n,value = NA, verbose = FALSE){

    if(verbose) cat(paste0('removing ',n,' points for the model (y) boundaryes ...\n'))

    if(n < 1) return(x) # nocov

    if(nlyr(x) == 1){ # for 2d rast
      A       <- matrix(values(x),
                        ncol  = ncol(x),
                        nrow  = nrow(x),
                        byrow = TRUE)
      A[1:n,] = value
      A[,1:n] = value
      A[(nrow(A)-n+1):nrow(A),] = value
      A[,(ncol(A)-n+1):ncol(A)] = value
      values(x) <- A
      return(x)
    }

    if(nlyr(x) >= 2){ # for 3d rast
      A       <- as.array(x)
      A[1:n,,] = value
      A[,1:n,] = value
      A[(nrow(A)-n+1):nrow(A),,] = value
      A[,(ncol(A)-n+1):ncol(A),] = value
      values(x) <- A
      return(x)
    }
  }

  model <- cut_boundary(mo, n = n, verbose = verbose)

  if(!skip_interp){
    if(verbose) cat('interpolating obs. (x) to model grid (y) ...\n')
    obser <- interp(x = ob, y = mo, method = method, mask = mask, verbose = verbose)
  }else{
    if(verbose) cat('skiping interpolation ...\n') # nocov
    obser <- ob                                    # nocov
  }

  if(!is.na(min)){
    if(verbose) cat('seting min value to',min,'\n')
    model[model < min] = NA
    obser[obser < min] = NA
  }
  if(!is.na(max)){
    if(verbose) cat('seting max value to',max,'\n')
    model[model > max] = NA
    obser[obser > max] = NA
  }

  model <- as.vector(model)
  obser <- as.vector(obser)

  if(length(model) < 1 | length(obser) < 1) stop('YOU DIED') # nocov

  if(missing(rname)){
    RESULT <- eval_function(model, obser, ...)
  }else{
    RESULT <- eval_function(model, obser, rname = rname, ...)
  }
  return(rbind(table,RESULT))
}
