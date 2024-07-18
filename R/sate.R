#' @title Functions to model evaluation using satellite
#'
#' @description functions to evaluate the spatial performance using satellite
#'
#' @return a data.frame
#'
#' @param mo rast with model
#' @param ob rast with observations
#' @param n number of points from the boundary removed, default is 5
#' @param min minimum value cutoff
#' @param max maximum value cutoff
#' @param rname passed to stat
#' @param method passed to terra::resample
#' @param eval_function evaluation function (default is stat)
#' @param verbose set TRUE to display additional information
#' @param ... other arguments passed to stat
#'
#' @note If a YOU DIED error message appears, means you are removing all the valid values using the arguments min or max.
#' @note If cate() is used for eval_function, the argument threshold must be included (see example).
#'
#' @import terra
#'
#' @examples
#' model_o3 <- terra::rast(paste0(system.file("extdata",package="eval3dmodel"),
#'                               "/camx_no2.Rds"))
#' omi_o3   <- terra::rast(paste0(system.file("extdata",package="eval3dmodel"),
#'                               "/omi_no2.Rds"))
#'
#' # generate the statistical indexes
#' sate(mo = model_o3,ob = omi_o3,rname = 'NO2_statistical')
#'
#' # generate categorical evaluation using 3.0 as threshold
#' sate(mo = model_o3,ob = omi_o3,rname = 'NO2_categorical',
#'      eval_function = cate, threshold = 3.0)
#'
#' @export

sate <- function(mo,ob,n = 6, min = NA, max = NA,rname, method = 'bilinear',
                 eval_function = stat, verbose = T, ...){

  if(missing(mo))
    stop('model input is missing!')

  if(missing(ob))
    stop('observation input is missing!')

  if(class(mo) %in% c('RasterLayer','RasterBrick')){
    mo <- rast(mo)
  }
  if(class(ob) %in% c('RasterLayer','RasterBrick')){
    ob <- rast(ob)
  }

  cut_boundary <- function(x, n,value = NA){

    if(n < 1) return(x)

    if(nlyr(x) == 1){ # for 2d rast
      A       <- matrix(values(x),
                        ncol  = ncol(x),
                        nrow  = nrow(x),
                        byrow = T)
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

  if(verbose) cat(paste0('removing ',n,' points for the model (y) boundaryes ...\n'))
  model <- cut_boundary(mo, n = n)
  if(verbose) cat('interpolating obs. (x) to model grid (y)...\n')
  obser <- interp(x = ob, y = mo, method = method, verbose = verbose)

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

  if(length(model) < 1 | length(obser) < 1) stop('YOU DIED')

  if(missing(rname)){
    return(eval_function(model = model, observation = obser, ...))
  }else{
    return(eval_function(model = model, observation = obser, rname = rname, ...))
  }
}
