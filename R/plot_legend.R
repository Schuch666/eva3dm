#' Plot a legend with the range of values
#'
#' @param x rast or array
#' @param y rast or array to mean (x is used only for the range in this case)
#' @param text.width Longitude in decimals
#' @param dig vector with number of digits for plot
#' @param xjust passed to legend
#' @param yjust passed to legend
#' @param horiz passed to legend
#' @param x.intersp passed to legend
#' @param y.intersp passed to legend
#' @param show.mean set TRUE to hide mean value
#' @param unit a string for units
#' @param label_mean label in case y is provided
#' @param ... extra arguments passed to legend
#'
#' @note for use with rast use before any change of projection
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

  if(class(x) %in% c('Raster','RasterLayer','RasterBrick')){
    x <- rast(x) # nocov
  }
  if(!missing(y)){
    if(class(y) %in% c('Raster','RasterLayer','RasterBrick')){ # nocov
      y <- rast(y) # nocov
    }
  }

  if(class(x) %in% c('SpatRaster')){
    x <- as.array(x)
  }
  if(!missing(y)){
    if(class(y) %in% c('SpatRaster')){ # nocov
      y <- as.array(y) # nocov
    }
  }

  suppressWarnings({
    mi <- paste('Min:', formatC(min(x, na.rm = TRUE), digits = dig[1], format = "f"),unit)
  })

  if(missing(y)){
    suppressWarnings({
      me <- paste('Mean:',formatC(mean(x,na.rm = TRUE), digits = dig[2], format = "f"),unit)
    })
  }else{
    suppressWarnings({
      me <- paste(label_mean,formatC(mean(y,na.rm = TRUE), digits = dig[2], format = "f"),unit)
    })
  }
  suppressWarnings({
    ma <- paste('Max:', formatC(max(x, na.rm = TRUE), digits = dig[3], format = "f"),unit)
  })

  if(show.mean){
    le <- c(mi,me,ma)
  }else{
    le <- c(mi,ma) # nocov
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
