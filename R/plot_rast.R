#' Plot rast (SpatRaster) object
#'
#' @description Custon plot for SpatRaster (terra R-package) object based on terra package
#'
#' @param r raster
#' @param color color scale
#' @param ncolor number of colors
#' @param log TRUE to plot in log-scale
#' @param proj TRUE to project the raster to lat-lon
#' @param plg list of parameters passed to terra::add_legend
#' @param pax list of parameters passed to graphics::axis
#' @param axe plot axis
#' @param grid add grid
#' @param latitude add a latitude axis
#' @param longitude  add a longitude axis
#' @param int interval of latitude and longitude lines
#' @param add_grid add a lat-lon grid (graticule style)
#' @param int_grig interval of grid lines
#' @param add_range add legend with max, average and min r values
#' @param ... arguments to be passing to terra::plot
#'
#' @import terra
#'
#' @export
#'
#' @examples
#' wrf <- paste(system.file("extdata", package = "eval3dmodel"),
#'                          "/wrfinput_d01", sep="")
#'
#' r <- wrf_rast(file=wrf, name='XLAT')
#'
#' plot_rast(r)
#'
plot_rast <- function(r,
                      color,
                      ncolor = 21,
                      log = FALSE,
                      proj = FALSE,
                      plg=list(tic = 'none',shrink=0.98),
                      pax=list(),
                      axe=FALSE,
                      grid=FALSE,
                      latitude = FALSE,
                      longitude = FALSE,
                      int = 10,
                      add_grid = FALSE,
                      grid_int = 10,
                      add_range = TRUE,
                      # log = FALSE,
                      # min,max,
                      # axe = !llaxis, llaxis = F,
                      # int = 10,
                      ...){

  if(missing(r))
    stop('r is missing!')

  if(missing(color)){
    color <- c("#08306B","#133A72","#1F4479","#2B4E81","#375888",
               "#436290","#4F6C97","#5B779E","#6781A6","#738BAD",
               "#7F95B4","#8B9FBC","#97A9C3","#A3B3CB","#AFBED2",
               "#BBC8DA","#C7D2E1","#D3DCE8","#DFE6F0","#EBF0F7",
               "#F7FBFF")
  }else{
    if(is.function(color)){
      color <- color(ncolor)
    }
  }

  extra <- function(){
    if(add_grid){
      lats  <- seq(-40,10,   by = grid_int)
      longs <- seq(-180,180, by = grid_int)
      terra::lines(terra::graticule(lon = longs,lat = lats,crs = terra::crs(r,proj=TRUE)),
                   lty = 3, col = "#666666",lwd = 1.2)
    }
    if(add_range)
      legend_range(as.matrix(r))
    terra::add_box()
  }

  if(proj){
    r <- project(r,"+proj=longlat +datum=WGS84 +no_defs")
  }

  p <- terra::plot(r, col = color, plg = plg, pax = pax,axe = axe, grid = FALSE, fun = extra, cex = cex, ...)
  .plot.latlon(x = p,int = int, proj = terra::crs(r,proj=TRUE))

  # if(hard_zlim & !log){
  #   r[r[] < zlim[1] ] = zlim[1]
  #   r[r[] > zlim[2] ] = zlim[2]
  # }

  # Rlog10 <- function(r,min){
  #   test <- suppressWarnings(log10(x = r))
  #   test[is.infinite(test)] <- min
  #   test[test[] < min ] = min
  #   return(test)
  # }
  #
  # if(log){
  #   r_log  <- Rlog10(r = r,min = min)
  #   rng    <- range(r_log[], na.rm = T)
  #   if(missing(max)){
  #     at     <- seq(round(rng[1], 1),round(rng[2], 1),by = 1)
  #   }else{
  #     at     <- seq(round(min, 1),round(max, 1),by = 1)
  #   }
  #   label <- paste0('10^',at)
  #   label <- parse(text = label)
  #   label[at == 0] = '  1'
  #
  #   arg <- list(at=at, labels=label)
  #
  #   if(missing(max)){
  #     plot(x             = r_log,
  #          legend.shrink = legend.shrink,
  #          legend.width  = legend.width,
  #          axe           = axe,
  #          axis.args     = arg,
  #          col           = col,
  #          ...)
  #   }else{
  #     plot(x             = r_log,
  #          legend.shrink = legend.shrink,
  #          legend.width  = legend.width,
  #          axe           = axe,
  #          axis.args     = arg,
  #          col           = col,
  #          zlim          = c(min,max),
  #          ...)
  #   }
  # }else{
  #   plot(x             = r,
  #        # legend.shrink = legend.shrink,
  #        # legend.width  = legend.width,
  #        # axe           = axe,
  #        # col           = col,
  #        # zlim          = zlim,
  #        ...)
  # }
  # if(llaxis){
  #   latitude(int = int)
  #   longitude(int = int)
  # }
}
