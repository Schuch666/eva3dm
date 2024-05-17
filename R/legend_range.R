#' Get the distance in kilometers between two points.
#'
#' @param x raster or array
#' @param y raster or array to mean (x is used only for the range in this case)
#' @param text.width Longitude in decimals
#' @param dig vector with number of digits for plot
#' @param xjust passed to legend
#' @param horiz passed to legend
#' @param x.intersp passed to legend
#' @param y.intersp passed to legend
#' @param show.mean set TRUE to hide mean value
#' @param unit a string for units
#' @param label_mean label in case y is provided
#' @param ... extra arguments passed to legend
#'
#' @note for use with raster use before any change of projection
#' @note text.width can vary depending on map dimensions
#'
#' @examples
#' x <- 1:10 + rnorm(10,sd = .4)
#' plot(x,ty='l')
#' legend_range(x)
#'
#' @export
#'
legend_range <- function(x, y,
                         text.width=NULL,
                         dig = c(2,2,2),
                         xjust = 0.005,
                         yjust = 0.95,
                         horiz = TRUE,
                         y.intersp =0.5,
                         x.intersp =0.5,
                         show.mean = TRUE,
                         unit = "",
                         label_mean = 'ALL:',
                         ...){

  # if(class(x)[1] == 'Raster' ||
  #    class(x)[1] == 'RasterLayer' ||
  #    class(x)[1] == 'RasterBrick'){
  #   x <- raster_to_ncdf(x)
  # }
  #
  # if(!missing(y)){
  #   if(class(y)[1] == 'Raster' ||
  #      class(y)[1] == 'RasterLayer' ||
  #      class(y)[1] == 'RasterBrick'){
  #     y <- raster_to_ncdf(y)
  #   }
  # }

  mi <- paste('Min:', formatC(min(x, na.rm = TRUE), digits = dig[1], format = "f"),unit)
  if(missing(y)){
    me <- paste('Mean:',formatC(mean(x,na.rm = TRUE), digits = dig[2], format = "f"),unit)
  }else{
    me <- paste(label_mean,formatC(mean(y,na.rm = TRUE), digits = dig[2], format = "f"),unit)
  }
  ma <- paste('Max:', formatC(max(x, na.rm = TRUE), digits = dig[3], format = "f"),unit)

  if(show.mean){
    le <- c(mi,me,ma)
  }else{
    le <- c(mi,ma)
  }

  terra::add_legend('bottomright',
                    legend     = le,
                    xjust      = xjust,
                    yjust      = yjust,
                    horiz      = horiz,
                    y.intersp  = y.intersp,
                    x.intersp  = x.intersp,
                    text.width = text.width,
                    ...)
}
