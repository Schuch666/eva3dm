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
#' @param latlon plot lat-lon axis (defoult), FALSE for r original axis
#' @param grid add grid (graticule style)
#' @param latitude add a latitude axis
#' @param longitude add a longitude axis
#' @param int interval of latitude and longitude lines
#' @param grid_int interval of grid lines
#' @param add_range add legend with max, average and min r values
#' @param log plot in log scale
#' @param zlim zlim
#' @param min minimum for log scale (defoul is -3)
#' @param max maximum for lo scale
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
                      proj = FALSE,
                      plg=list(tic = 'none',shrink=0.98),
                      pax=list(),
                      latlon=TRUE,
                      grid=TRUE,
                      latitude = TRUE,
                      longitude = TRUE,
                      int = 10,
                      grid_int = 10,
                      add_range = TRUE,
                      log = FALSE,
                      zlim,
                      min = -3,
                      max,
                      ...){

  if(missing(r))
    stop('r is missing!')

  if(!missing(zlim) & !log){
    r2 <- r
    r2[r[] < zlim[1] ] = zlim[1]
    r2[r[] > zlim[2] ] = zlim[2]
  }else{
    r2 <- r
  }

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

  e_o     <- ext(r)
  Points  <- vect(cbind(x = e_o[1:2], y = e_o[3:4]),
                  type="points",
                  crs =  terra::crs(r,proj=TRUE))
  proj_p  <- project(Points,"+proj=longlat +datum=WGS84 +no_defs")
  e_p     <- ext(proj_p)
  min_lon <- as.numeric(e_p[1])
  max_lon <- as.numeric(e_p[2])
  min_lat <- as.numeric(e_p[3])
  max_lat <- as.numeric(e_p[4])

  vet_lon <- seq(-80,80,by = int)
  vet_lon <- vet_lon[vet_lon > min_lon - int & vet_lon < max_lon + int]
  vet_lat <- seq(-180,180,by = int)
  vet_lat <- vet_lat[vet_lat > min_lat - int & vet_lat < max_lat + int]

  extra <- function(){
    if(grid){
      terra::lines(terra::graticule(lon = vet_lon,lat = vet_lat,
                                    crs = terra::crs(r,proj=TRUE)),
                   lty = 3, col = "#666666",lwd = 1.2)
    }
    if(add_range)
      legend_range(r)
    terra::add_box()
  }

  if(proj){
    r <- project(r,"+proj=longlat +datum=WGS84 +no_defs")
  }

  if(log){
    Rlog10 <- function(r,min){
      test <- suppressWarnings(log10(x = r))
      test[is.infinite(test)] <- min
      test[test[] < min ] = min
      return(test)
    }

    r_log  <- Rlog10(r = r,min = min)

    if(missing(max)){
      max <- as.numeric(global(r_log,'mean', na.rm = TRUE))
    }else{
      r_log[r_log[] > max ] = max
    }

    at    <- seq(round(min, 1),round(max, 1),by = 1)
    at    <- at[at <= as.numeric(global(r_log,'max', na.rm = TRUE))]
    at    <- at[at >= as.numeric(global(r_log,'min', na.rm = TRUE))]

    label <- paste0('10^',at)
    label <- parse(text = label)
    label[at == 0] = '  1'

    arg <- list(at=at, labels=label)

    p <- terra::plot(r_log, col = color, plg = c(plg,arg), pax = pax,axe = !latlon, grid = FALSE,fun = extra, ...)
    if(latlon) .plot.latlon(x = p,proj = terra::crs(r,proj=TRUE),int = int,e = e_o)
  }else{
    p <- terra::plot(r2, col = color, plg = plg, pax = pax,axe = !latlon, grid = FALSE, fun = extra, ...)
    if(latlon) .plot.latlon(x = p,proj = terra::crs(r,proj=TRUE),int = int,e = e_o)
  }
}
