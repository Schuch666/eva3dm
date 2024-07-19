#' Plot rast (SpatRaster) object
#'
#' @description Custon plot for SpatRaster (terra R-package) object based on terra package
#'
#' @param r raster
#' @param color color scale
#' @param ncolor number of colors
#' @param proj TRUE to project the raster to lat-lon
#' @param plg list of parameters passed to terra::add_legend
#' @param pax list of parameters passed to graphics::axis
#' @param latitude add a latitude axis
#' @param longitude add a longitude axis
#' @param int interval of latitude and longitude lines
#' @param grid add grid (graticule style)
#' @param grid_int interval of grid lines
#' @param grid_col color for grid lines
#' @param add_range add legend with max, average and min r values
#' @param ndig number of digits for legend_range
#' @param range range to plot
#' @param log TRUE to plot in log-scale
#' @param min minimum for log scale (defoul is -3)
#' @param max maximum for log scale
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
                      latitude = TRUE,
                      longitude = TRUE,
                      int = 10,
                      grid=TRUE,
                      grid_int = int,
                      grid_col = "#666666",
                      add_range = TRUE,
                      ndig = 2,
                      log = FALSE,
                      range,
                      min = -3,
                      max,
                      ...){

  if(missing(r))
    stop('r is missing!')

  if(latitude | longitude){
    latlon = TRUE
  }else{
    latlon = FALSE
  }

  if(!missing(range) & !log){
    r2 <- r
    r2[r[] < range[1] ] = range[1]
    r2[r[] > range[2] ] = range[2]
  }else{
    r2 <- r
  }

  if(missing(range))
    range <- as.numeric(global(r2,'range'))

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

  if(latlon){
    ax  <- latlon(r = r,int = int,e = e_o)
    if(latitude & !longitude){
      ax$side <- 2
    }
    if(longitude & !latitude){
      ax$side <- 1
    }
    pax <- c(pax,ax)
  }

  extra <- function(){
    if(grid){
      terra::lines(terra::graticule(lon = vet_lon,lat = vet_lat,
                                    crs = terra::crs(r,proj=TRUE)),
                   lty = 3, col = grid_col,lwd = 1.2)
    }
    if(add_range)
      legend_range(r,dig = c(ndig,ndig,ndig))
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

    terra::plot(r_log, col = color, plg = c(plg,arg), pax = pax,axe = T,
                grid = FALSE,fun = extra, range = range, ...)
  }else{
    terra::plot(r2, col = color, plg = plg, pax = pax,axe = T,
                grid = FALSE,fun = extra, range = range, ...)
  }
}

#' @import terra
#'
latlon <- function(r,int,e,tn = 100) {

  proj <-  terra::crs(r,proj=TRUE)

  # latitude
  vet_lon <- seq(-80,80,by = int)
  lab_lon <- c(paste0(seq(80,int,by=-int),"\u00baS"),'0',
               paste0(seq(int,80,by=int),"\u00baN"))

  usr <- ext(r)
  tx  <- rep(unname(usr[1]), tn)
  ty  <- seq(unname(usr[3]), unname(usr[4]), length.out=tn)

  firstPoints  <- vect(cbind(x = tx, y = ty), type="points",crs = proj)
  tt           <- project(firstPoints,"+proj=longlat +datum=WGS84 +no_defs")
  axis_coords  <- crds(tt)
  tfcn         <- custom_approxfun(axis_coords[,2], ty)

  # longitude
  vet_lat <- seq(-180,180,by = int)
  lab_lat <- c(paste0(seq(180,int,by=-int),"\u00baW"),'0',
               paste0(seq(int,180,by=int),"\u00baE"))

  tx <- seq(unname(usr[1]), unname(usr[2]), length.out = tn)
  ty <- rep(unname(usr[3]), tn)

  firstPoints  <- vect(cbind(x = tx, y = ty), type="points",crs = proj)
  tt           <- project(firstPoints,"+proj=longlat +datum=WGS84 +no_defs")
  axis_coords  <- crds(tt)
  tfcn2        <- custom_approxfun(axis_coords[,1], tx)

  min_lon <- as.numeric(e[1])
  max_lon <- as.numeric(e[2])
  min_lat <- as.numeric(e[3])
  max_lat <- as.numeric(e[4])

  valid_lat <- tfcn2(vet_lat)
  valid_lon <- tfcn(vet_lon)

  valid_lon[ valid_lon >= max_lat ] = NA
  valid_lon[ valid_lon <= min_lat ] = NA
  valid_lat[ valid_lat >= max_lon ] = NA
  valid_lat[ valid_lat <= min_lon ] = NA

  yat  <- valid_lon
  ylab <- lab_lon
  xat  <- valid_lat
  xlab <- lab_lat

  return(list(side = 1:2,
              xat=xat, xlabs=xlab,
              yat=yat, ylabs=ylab))
}

custom_approxfun <- function(x, y, method = "linear", rule = 1) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  if (any(diff(x) <= 0)) {
    stop("x must be strictly increasing")
  }

  interpolate <- function(new_x) {
    n <- length(x)
    if (method != "linear") {
      stop("Only linear interpolation is supported")
    }

    if (any(new_x < x[1]) || any(new_x > x[n])) {
      if (rule == 1) {
        new_x <- pmax(pmin(new_x, x[n]), x[1])
      } else {
        stop("new_x values out of bounds and rule != 1")
      }
    }

    y_new <- numeric(length(new_x))

    for (i in seq_along(new_x)) {
      if (new_x[i] <= x[1]) {
        y_new[i] <- y[1]
      } else if (new_x[i] >= x[n]) {
        y_new[i] <- y[n]
      } else {
        j <- which(x <= new_x[i])
        j <- j[length(j)]
        y_new[i] <- y[j] + (y[j + 1] - y[j]) * (new_x[i] - x[j]) / (x[j + 1] - x[j])
      }
    }
    return(y_new)
  }

  return(interpolate)
}
