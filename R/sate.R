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
#' @param verbose set TRUE to display additional information
#'
#' @import terra
#'
#' @export

sate <- function(mo,ob,n = 6, min = NA, max = NA,rname,verbose = T){

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

  if(verbose) cat(paste0('removing ',n,' points for the model (y) boundaryes ...\n'))
  model <- cut_boundary(mo, n = n)
  if(verbose) cat('interpolating obs. (x) to model grid (y)...\n')
  obser <- interp(x = ob, y = mo, verbose = verbose)

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

  if(missing(rname)){
    return(stat(model = model, observation = obser))
  }else{
    return(stat(model = model, observation = obser, rname = rname))
  }
}
