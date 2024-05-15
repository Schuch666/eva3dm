#' Plot rast object
#'
#' @description functions that modified plot from raster package
#'
#' @param r raster
#' @param color color scale
#' @param ncolor number of colors
#' @param log TRUE to plot in log-scale
#' @param proj TRUE to project the raster to latlon
#' @param plg list of parameters passed to terra::add_legend
#' @param pax list of parameters passed to graphics::axis
#' @param ... arguments to be passing to terra::plot
#'
#' @import terra
#'
#' @export
#'
#' @examples
#' m <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/model.Rds"))
#'
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
                      latitude = TRUE,
                      longitude = TRUE,
                      add_grid = TRUE,
                      add_range = TRUE,
                      # log = FALSE,
                      # min,max,
                      # axe = !llaxis, llaxis = F,
                      # int = 10,
                      ...){

  if(missing(color)){
    color <- colorRampPalette(c("#08306b", "#f7fbff"))(ncolor)
  }else{
    if(is.function(color)){
      color <- color(ncolor)
    }
  }
  extra <- function(){
    R <- raster(r)
    if(latitude)
      hackWRF::latitude_proj(R)
    if(longitude)
      hackWRF::longitude_proj(R,line = -0.6)
    if(add_grid)
      hackWRF::grid_proj(R,lat_max = 0,lat_min = -30)
    if(add_range)
      hackWRF::legend_range(R)
  }

  if(proj){
    r <- project(r,"+proj=longlat +datum=WGS84 +no_defs")
  }

  terra::plot(r, col = color, plg = plg, pax = pax,axe = axe, grid = grid, fun = extra, ...)

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
