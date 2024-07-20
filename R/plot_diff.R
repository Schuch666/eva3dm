#' Plot rast (SpatRaster) object
#'
#' @description Custon diffrerence plots (x - y) of plot for SpatRaster (terra R-package) object based on terra package
#'
#' @param x SpatVector points
#' @param y values to plot
#' @param col color
#' @param absolute to plot absolute difference
#' @param relative to plot relative difference
#' @param lim_1 range of values for scale
#' @param lim_2 calculate symmetrical scale
#' @param units units annotation
# ' @param file name to save a .png file instead of plot
# ' @param w width for png function
# ' @param h height for png function
# ' @param pt fond size for png function
#' @param ... arguments to be passing to terra::plot
#'
#' @import terra
#'
#' @export
#'
#' @examples
#' print('oi')
#'

plot_diff <- function(x,y,col,
                      absolute = T,
                      relative = T,
                      lim_1 = NA, lim_2 = NA,
                      units = c('',expression("%")),
                      # file,
                      # w = 800, h = 1200, pt = 18,
                      ...){

  if(missing(col))
    col <- c("#1B2C62","#204385","#265CA9","#4082C2",
             "#5DA9DB","#80C4EA","#A4DDF7","#C1E7F8",
             "#DEF2FA","#F2FAFD","#FFFFFF","#FFFFFF",
             "#FEFAE6","#FDF0B4","#FDDA7C","#FEBC48",
             "#FB992F","#F7762B","#E84E29","#D72828",
             "#B81B22","#97161A","#921519")

  diff <- x - y
  rel  <- 100 * diff / (y + 0.00000000001)

  if(is.na(lim_1))
    lim_1 = as.numeric(global(diff,'range',na.rm = T))
  if(is.na(lim_2))
    lim_2 = as.numeric(global(rel,'range',na.rm = T))

  lim_1[1] <- -max(abs(range(lim_1)))
  lim_1[2] <- max(abs(range(lim_1)))

  lim_2[1] <- -max(abs(range(lim_2)))
  lim_2[2] <- max(abs(range(lim_2)))

  # if(!missing(file)){
  #   cat('saving plot_diff in',file,'...\n')
  #   grDevices::png(filename = file,
  #                  width = w,
  #                  height = h,
  #                  pointsize = pt)
  # }

  # if(absolute & relative){
  #   p <- graphics::par(mfrow=c(2,1))
  #   on.exit(graphics::par(p))
  # }

  if(absolute){
    plot_rast(diff, color = col,range = lim_1,
              plg = list(tic = 'none',shrink=0.98, title = units[1]))
    # terra::add_mtext(text = units[1], side = 3, line=0.0, adj = 1)
    # mtext(units[1], side=3, line=2.2,adj = 1)
  }
  if(relative){
    plot_rast(rel, color = col,range = lim_2,
              plg = list(tic = 'none',shrink=0.98, title = units[2]))
    # terra::add_mtext(text = units[2], side = 3, line=0.0, adj = 1)
    # mtext(units[2], side=3, line=2.2,adj = 1)
  }

  # if(!missing(file))
  #   garbage <- grDevices::dev.off()
}
