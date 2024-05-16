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
#' @param add_grid add a lat-lon grid (graticule style)
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
                      add_grid = FALSE,
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
    # colorRampPalette(c("#08306b", "#f7fbff"))(21)
  }else{
    if(is.function(color)){
      color <- color(ncolor)
    }
  }

  # longitude_proj <- function(proj, int = 10, side = 1,lmin = -180, lmax = 180, ...){
  #   vet_lat <- seq(lmin,lmax,by = int)
  #   lab_lat <- c(paste0(seq(-lmin,int,by=-int),"\u00baW"),'0',
  #                paste0(seq(int,lmax,by=int),"\u00baE"))
  #
  #   usr <- par('usr')
  #   tn <- 100
  #   tx <- seq(usr[1], usr[2], length.out = tn)
  #   ty <- rep(usr[3], tn)
  #
  #   pontos           <- cbind(x = tx, y = ty)
  #   firstPoints      <- SpatialPoints(coords = pontos)
  #   crs(firstPoints) <- proj
  #   tt               <- spTransform(x = firstPoints,
  #                                   CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  #   axis_coords      <- coordinates(tt)
  #   tfcn             <- approxfun(axis_coords[,1], tx)
  #
  #   axis(side,at = tfcn(vet_lat),labels = lab_lat, ... )
  # }
  #
  # latitude_proj <- function(proj, int = 10,side = 2,lmin = -80, lmax = 80, ...){
  #   vet_lon <- seq(lmin,lmax,by = int)
  #   lab_lon <- c(paste0(seq(-lmin,int,by=-int),"\u00baS"),'0',
  #                paste0(seq(int,lmax,by=int),"\u00baN"))
  #
  #   usr <- par('usr')
  #   tn  <- 100
  #   tx  <- rep(usr[1], tn)
  #   ty  <- seq(usr[3], usr[4], length.out=tn)
  #
  #   # print(tx)
  #   # print(ty)
  #   #
  #   # print(usr)
  #   #
  #   # e <- unlist(terra:::get.clip())
  #   # print(e)
  #   # print(class(e))
  #   #
  #   # tn  <- 100
  #   # tx  <- rep(as.numeric(e[1]), tn)
  #   # ty  <- seq(as.numeric(e[3]), as.numeric(e[4]), length.out=tn)
  #   #
  #   # print(tx)
  #   # print(ty)
  #
  #   pontos           <- cbind(x = tx, y = ty)
  #   firstPoints      <- SpatialPoints(coords = pontos)
  #   crs(firstPoints) <- proj
  #   tt               <- spTransform(x = firstPoints,
  #                                   CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  #   axis_coords      <- coordinates(tt)
  #   tfcn             <- approxfun(axis_coords[,2], ty)
  #
  #   axis(side,at = tfcn(vet_lon),labels = lab_lon, ... )
  # }

  # grid_proj <- function(proj, int = 10, lty = 3, col = "#666666",
  #                       lat_min = -80,  lat_max = 80,
  #                       lon_min = -160, lon_max = 160,
  #                       ...){
  #
  #   for(lat in seq(lat_min,lat_max,by = int)){
  #     M          <- matrix(data = lat, nrow = 1000/int, ncol = 2)
  #     M[,1]      <- seq(-160,160,along.with = M[,1])
  #     line1      <- Line(M)
  #     linea      <- Lines(line1, ID = "a")
  #     firstLine  <- SpatialLines(LinesList = list(linea))
  #     crs(firstLine) <- "+proj=longlat +datum=WGS84 +no_defs"
  #     firstLine_proj <- spTransform(x = firstLine, CRSobj = CRS(proj))
  #     terra::lines(firstLine_proj, lty = lty, col = col,...)
  #   }
  #   for(lon in seq(lon_min,lon_max,by = int)){
  #     M          <- matrix(data = lon, nrow = 2000/int, ncol = 2)
  #     M[,2]      <- seq(-80,80,along.with = M[,2])
  #     line1      <- Line(M)
  #     linea      <- Lines(line1, ID = "a")
  #     firstLine  <- SpatialLines(LinesList = list(linea))
  #     crs(firstLine) <- "+proj=longlat +datum=WGS84 +no_defs"
  #     firstLine_proj <- spTransform(x = firstLine, CRSobj = CRS(proj))
  #     terra::lines(firstLine_proj, lty = lty, col = col,...)
  #   }
  # }

  extra <- function(){
    proj <- crs(r,proj=TRUE)
    # if(latitude)
    #   latitude_proj(proj)
    # if(longitude)
    #   longitude_proj(proj)
    # if(add_grid)
    #   grid_proj(proj,lat_max = 0,lat_min = -30)
    # if(add_range)
    #   legend_range(as.matrix(r))
    # terra::add_box()
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
