#' Plot rast (SpatRaster) object
#'
#' @description Custon plot for SpatRaster (terra R-package) object based on terra package
#'
#' @param r raster
#' @param color color scale, or name of a custom color scale (see notes)
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
#' @param range range of original values to plot
#' @param log TRUE to plot in log-scale
#' @param min minimum log value for log scale (defoul is -3)
#' @param max maximum log value for log scale
#' @param unit title for color bar (defoult is )
#' @param ... arguments to be passing to terra::plot
#'
#' @return No return value
#'
#' @import terra
#'
#' @note color scale includes: 'eva3r' (default), 'eva4', 'blues' and 'diff'
#'
#' @export
#'
#' @examples
#' wrf <- paste(system.file("extdata", package = "eva3dm"),
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
                      plg=list(tic = 'none',shrink=1.00),
                      pax=list(),
                      latitude = TRUE,
                      longitude = TRUE,
                      int = 10,
                      grid = FALSE,
                      grid_int = int,
                      grid_col = "#666666",
                      add_range = FALSE,
                      ndig = 2,
                      log = FALSE,
                      range,
                      min = -3,
                      max,
                      unit,
                      ...){

  if(missing(r))
    stop('r is missing!') # nocov

  if(log & any(global(r,'max',na.rm = TRUE) <= 0))
    stop('r is non-positive, no plot is generated') # nocov

  if(latitude | longitude){
    latlon = TRUE  # nocov
  }else{
    latlon = FALSE # nocov
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
    color <- 'eva3r'
  }else{
    if(is.function(color)){  # nocov
      color <- color(ncolor) # nocov
    }
  }
  if(color[1] == 'eva3r')
    # eva 3 colors reverse
    color <- rev(c("#AD7AD7","#A670CF","#9F67C7","#995EBF","#9255B7",
                   "#8C4CAF","#8542A7","#7F39A0","#783098","#722790",
                   "#6E228C","#732E97","#783AA2","#7D47AD","#8253B9",
                   "#875FC4","#8C6BCF","#9178DA","#9684E6","#9B90F1",
                   "#9C95F4","#948DED","#8D86E5","#857EDD","#7E77D5",
                   "#766FCE","#6F68C6","#6760BE","#6059B6","#5851AF",
                   "#5B57A8","#6365A1","#6B739A","#748294","#7C908D",
                   "#849E86","#8DAC80","#95BA79","#9DC872","#A6D76C",
                   "#A1D466","#99CC60","#90C55B","#88BE55","#80B650",
                   "#78AF4A","#6FA844","#67A03F","#5F9939","#579234"))
  # nocov start
  if(color[1] == 'eva4')
    # eva 4 colors
    color <- c("#AD7AD7","#A36DCC","#9A60C1","#9153B6","#8846AB",
               "#7F39A0","#762C94","#6D208A","#743199","#7B42A9",
               "#8253B9","#8964C8","#9075D8","#9786E8","#9F98F8",
               "#948DED","#8A83E2","#7F78D7","#756ECC","#6A63C1",
               "#6059B6","#564FAC","#6162A2","#6D7699","#798A90",
               "#849E86","#90B27D","#9CC674","#A8DA6B","#9CCF63",
               "#90C55B","#85BB53","#79B04B","#6EA643","#629C3B",
               "#579234","#6D9838","#839F3C","#9AA540","#B0AC45",
               "#C7B249","#DDB94D","#F4C052","#EDB44D","#E6A948",
               "#DF9D43","#D9923E","#D28639","#CB7B34","#C5702F")

  if(color[1] == 'blues')
    # desaturated blue
    color <- c("#08306B","#133A72","#1F4479","#2B4E81","#375888",
               "#436290","#4F6C97","#5B779E","#6781A6","#738BAD",
               "#7F95B4","#8B9FBC","#97A9C3","#A3B3CB","#AFBED2",
               "#BBC8DA","#C7D2E1","#D3DCE8","#DFE6F0","#EBF0F7",
               "#F7FBFF")

  if(color[1] == 'bluesr')
    # desaturated blue
    color <- rev(c("#08306B","#133A72","#1F4479","#2B4E81","#375888",
                   "#436290","#4F6C97","#5B779E","#6781A6","#738BAD",
                   "#7F95B4","#8B9FBC","#97A9C3","#A3B3CB","#AFBED2",
                   "#BBC8DA","#C7D2E1","#D3DCE8","#DFE6F0","#EBF0F7",
                   "#F7FBFF"))

  if(color[1] == 'diff')
    # diff including dark_blue - cyan - white - orange - dark_red
    color <- c("#1B2C62","#204385","#265CA9","#4082C2",
               "#5DA9DB","#80C4EA","#A4DDF7","#C1E7F8",
               "#DEF2FA","#F2FAFD","#FFFFFF","#FFFFFF",
               "#FEFAE6","#FDF0B4","#FDDA7C","#FEBC48",
               "#FB992F","#F7762B","#E84E29","#D72828",
               "#B81B22","#97161A","#921519")
  # nocov end
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

  delta_x     <- abs(max_lat - min_lat)
  delta_y     <- abs(max_lon - min_lon)
  small_delta <- min(delta_x,delta_y)

  if(int >= 1.1 * small_delta){
    int      = pretty(x = small_delta,n = 12)[1]
    grid_int = int
  }

  vet_lon <- seq(-80,80,by = int)
  vet_lon <- vet_lon[vet_lon >= min_lon - 2*int & vet_lon <= max_lon + 2*int]
  vet_lat <- seq(-180,180,by = int)
  vet_lat <- vet_lat[vet_lat >= min_lat - 2*int & vet_lat <= max_lat + 2*int]

  if(latlon){
    ax  <- latlon(r = r,int = int,e = e_o)
    if(latitude & !longitude){
      ax$side <- 2 # nocov
    }
    if(longitude & !latitude){
      ax$side <- 1 # nocov
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

  if(missing(unit)){
    plg = c(plg,list(title = terra::units(r)[1]))
  }else{
    plg = c(plg,list(title = unit))
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
      max <- as.numeric(global(r_log,'max', na.rm = TRUE))
    }else{
      r_log[r_log[] > max ] = max
    }

    at    <- seq(round(min, 1),round(max, 1),by = 1)

    label <- paste0('10^',at)
    label <- parse(text = label)
    label[at == 0] = '  1'

    arg <- list(at=at, labels=label)

    terra::plot(r_log, col = color,
                plg = c(plg,arg), pax = pax,
                axe = TRUE,
                grid = FALSE,fun = extra,
                range = c(min,max),
                ...)
  }else{
    a = as.numeric( terra::global(r2,'max',na.rm = TRUE) )
    b = as.numeric( terra::global(r2,'min',na.rm = TRUE) )
    if(!is.na(a) & a == b)
      plg=list()
    terra::plot(r2, col = color, plg = plg, pax = pax,axe = TRUE,
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
    stop("x and y must have the same length") # nocov
  }
  if (any(diff(x) <= 0)) {
    stop("x must be strictly increasing") # nocov
  }

  interpolate <- function(new_x) {
    n <- length(x)
    if (method != "linear") {
      stop("Only linear interpolation is supported") # nocov
    }

    if (any(new_x < x[1]) || any(new_x > x[n])) {
      if (rule == 1) {
        new_x <- pmax(pmin(new_x, x[n]), x[1])
      } else {
        stop("new_x values out of bounds and rule != 1") # nocov
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
