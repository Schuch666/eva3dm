#' @title Interpolation (project and resample)
#'
#' @description function to project and interpolate rast
#'
#' @param x rast to be interpolated
#' @param y target rast of the interpolation
#' @param method passed to terra::resample
#' @param verbose display additional information (not used)
#'
#' @import terra
#'
#' @examples
#' model_o3 <- terra::rast(paste0(system.file("extdata",package="eva3dm"),
#'                               "/camx_no2.Rds"))
#' omi_o3   <- terra::rast(paste0(system.file("extdata",package="eva3dm"),
#'                               "/omi_no2.Rds"))
#'
#' # interpolate omi O3 column to model grid
#' omi_o3c_interp_model <- interp(omi_o3,model_o3)
#'
#' # interpolate model o3 column to omi grid
#' model_o3c_interp_omi <- interp(omi_o3,model_o3)
#'
#' @export
interp <- function(x,y,method = 'bilinear',verbose = F){
  if(class(x) %in% c('RasterLayer','RasterBrick')){
    if(verbose)                      # nocov
      cat('converting x to rast\n')  # nocov
    x <- rast(x)                     # nocov
  }
  if(class(y) %in% c('RasterLayer','RasterBrick')){
    if(verbose)                      # nocov
      cat('converting y to rast\n')  # nocov
    y <- rast(y)                     # nocov
  }
  x_proj <- terra::project(x,terra::crs(y,proj=TRUE))
  x_res  <- terra::resample(x_proj,y,method = method)
  return(x_res)
}
