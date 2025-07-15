#' Plot the difference from two SpatRaster objects
#'
#' @description Custom difference (x - y) plots for SpatRaster object (based on terra package)
#'
#' @param x SpatVector points
#' @param y values to plot
#' @param col color
#' @param absolute to plot absolute difference
#' @param relative to plot relative difference
#' @param lim_a range of values for absolute scale
#' @param lim_r range of values for relative scale
#' @param scale variable multiplier for absolute difference
#' @param unit annotation for units
#' @param fill filling NAs
#' @param ... arguments to be passing to plot_raster
#'
#' @return No return value
#'
#' @import terra
#'
#' @export
#'
#' @examples
#'
#' folder <- system.file("extdata",package="eva3dm")
#' wrf    <- paste0(folder,"/wrfinput_d01")
#' A      <- wrf_rast(wrf,'XLAT')
#' terra::units(A) <- 'degrees'
#' B      <- wrf_rast(wrf,'XLONG')
#' plot_diff(A,B,int = 2)

plot_diff <- function(x,y,col,
                      absolute = TRUE,
                      relative = TRUE,
                      lim_a = NA, lim_r = NA, scale,
                      unit = c(units(x),expression("%")),
                      fill = FALSE,
                      ...){

  if(missing(col))
    col <- c("#1B2C62","#204385","#265CA9","#4082C2",
             "#5DA9DB","#80C4EA","#A4DDF7","#C1E7F8",
             "#DEF2FA","#F2FAFD","#FFFFFF","#FFFFFF",
             "#FEFAE6","#FDF0B4","#FDDA7C","#FEBC48",
             "#FB992F","#F7762B","#E84E29","#D72828",
             "#B81B22","#97161A","#921519")

  diff <- x - y

  if(absolute){
    if(is.na(lim_a))
      lim_a = as.numeric(global(diff,'range',na.rm = TRUE))

    lim_a[1] <- -max(abs(range(lim_a)))
    lim_a[2] <- max(abs(range(lim_a)))

    plot_rast(diff, color = col,range = lim_a, scale = scale,
              plg = list(tic = 'none', shrink=0.98, title = unit[1]),
              fill = fill, ...)
  }
  if(relative){
    if(fill){
      rel  <- 100 * diff / y
    }else{
      rel  <- 100 * diff / (y + 0.00000000001)
    }

    if(is.na(lim_r))
      lim_r = as.numeric(global(rel,'range',na.rm = TRUE))

    lim_r[1] <- -max(abs(range(lim_r)))
    lim_r[2] <- max(abs(range(lim_r)))

    plot_rast(rel, color = col,range = lim_r,
              plg = list(tic = 'none', shrink=0.98, title = unit[2]),
              fill = fill, ... )
  }
}
