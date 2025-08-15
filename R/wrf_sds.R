#' Creates SpatRasterDataset object from wrf file
#'
#' @description Creates a SpatRasterDataset (terra R-package) object from a variable from wrf file (or another compatible NetCDF) for all times and levels
#'
#' @param file wrf file
#' @param name variable name
#' @param map (optional) file with lat-lon variables and grid information
#' @param latlon logical (default is FALSE), set TRUE project the output to "+proj=longlat +datum=WGS84 +no_defs"
#' @param method method passed to terra::projection, default is bilinear
#' @param flip_h horizontal flip (by rows)
#' @param flip_v vertical flip (by cols)
#' @param verbose display additional information
#' @param ... extra arguments passed to ncdf4::ncvar_get
#'
#' @return SpatRasterDataset object (from terra package), each time-step is considered one subdatasets and each layer is one nlyr.
#'
#' @note The convention adopted to select specific times and atmospheric layers on wrf_sds is sds[time,layer] to keep consistence with sds.
#'
#' @import terra ncdf4
#' @importFrom utils menu
#'
#' @examples
#' file <- paste0(system.file("extdata",package="eva3dm"),"/wrf_4d_o3_Boston.nc")
#' O34d <- wrf_sds(file,'o3',verbose = TRUE)
#'
#' # selecting one time, keeping multiple layers
#' O34d[1,]
#'
#' # selecting one layer, keeping multiple times
#' O34d[,1]
#'
#' @export
#'

wrf_sds <- function(file = file.choose(),
                    name = NA,
                    map,
                    latlon = FALSE,
                    method = 'bilinear',
                    flip_h = FALSE,
                    flip_v = FALSE,
                    verbose = FALSE,
                    ...){

  if(!is.na(name)){
    if(name == 'time'){
      wrf <- ncdf4::nc_open(file)                                                       # nocov
      if(verbose)                                                                       # nocov
        cat(paste0('reading Times from ', file,'\n'))                                   # nocov
      TIME <- ncdf4::ncvar_get(wrf,'Times')                                             # nocov
      TIME <- as.POSIXlt(TIME, tz = "UTC", format="%Y-%m-%d_%H:%M:%OS", optional=FALSE) # nocov
      if(verbose)                                                                       # nocov
        cat('returning Times in POSIXct\n')                                             # nocov
      return(TIME)                                                                      # nocov
    }
  }

  wrf <- ncdf4::nc_open(file)
  if(is.na(name)){                                                  # nocov start
    name  <- menu(names(wrf$var), title = "Choose the variable:")
    POL   <- ncdf4::ncvar_get(wrf, names(wrf$var)[name], ... )
    name  <- names(wrf$var)[name]                                     # nocov end
  }else{
    POL   <- ncdf4::ncvar_get(wrf,name, ... )
  }
  if(verbose) cat(paste0('reading ',name,' from ', file,'\n'))            # nocov
  if(verbose) cat(paste("creating SpatRasterDataset for",name,'\n'))      # nocov

  if(missing(map)){                                                       # nocov
    coord_file = file                                                     # nocov
  }else{                                                                  # nocov
    coord_file = map                                                      # nocov
    if(verbose) cat('using coods and grid information from',map,'file\n') # nocov
  }

  coordNC <- tryCatch(suppressWarnings(ncdf4::nc_open(coord_file)),
                      error=function(cond) {message(cond); return(NA)})  # nocov

  coordvarList = names(coordNC[['var']])
  if ("XLONG_M" %in% coordvarList & "XLAT_M" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG_M")                      # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT_M")                       # nocov
  } else if ("XLONG" %in% coordvarList & "XLAT" %in% coordvarList) {
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG")
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT")
  } else if ("lon" %in% coordvarList & "lat" %in% coordvarList) {            # nocov
    inNCLon <- ncdf4::ncvar_get(coordNC, "lon")                              # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "lat")                              # nocov
  } else if ("longitude" %in% coordvarList & "latitude" %in% coordvarList) { # nocov
    inNCLon <- ncdf4::ncvar_get(coordNC, "longitude")                        # nocov
    inNCLat <- ncdf4::ncvar_get(coordNC, "latitude")                         # nocov
  } else {
    stop('Error: Latitude and longitude fields not found (tried: XLAT_M/XLONG_M, XLAT/XLONG, lat/lon longitude/latitude') # nocov
  }

  nrows <- dim(inNCLon)[2]
  ncols <- dim(inNCLon)[1]

  # Reverse column order to get UL in UL
  if(length(dim(inNCLon)) == 3){ # for special case of lat/lon has more dimensions
    x <- as.vector(inNCLon[, ncol(inNCLon):1,])
    y <- as.vector(inNCLat[, ncol(inNCLat):1,])
  }else{
    x <- as.vector(inNCLon[,ncol(inNCLon):1])   # nocov
    y <- as.vector(inNCLat[,ncol(inNCLat):1])   # nocov
  }
  rm(inNCLon,inNCLat)

  # Get geogrid and projection info
  map_proj <- ncdf4::ncatt_get(coordNC, varid=0, attname="MAP_PROJ")$value
  cen_lat  <- ncdf4::ncatt_get(coordNC, varid=0, attname="CEN_LAT")$value
  cen_lon  <- ncdf4::ncatt_get(coordNC, varid=0, attname="CEN_LON")$value
  truelat1 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT1")$value
  truelat2 <- ncdf4::ncatt_get(coordNC, varid=0, attname="TRUELAT2")$value
  ref_lon  <- ncdf4::ncatt_get(coordNC, varid=0, attname="STAND_LON")$value

  if(map_proj == 1){
    geogrd.proj <- paste0("+proj=lcc +lat_1=", truelat1,
                          " +lat_2=", truelat2,
                          " +lat_0=", cen_lat,
                          " +lon_0=", ref_lon,
                          " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
  } else if(map_proj == 2){                              # nocov
    if(cen_lat > 0){                                     # nocov
      hemis =  90                                        # nocov
    }else{                                               # nocov
      hemis = -90                                        # nocov
    }
    geogrd.proj <- paste0("+proj=stere +lat_0=",hemis,   # nocov
                          " +lon_0=",ref_lon,            # nocov
                          " +lat_ts=",truelat1,          # nocov
                          " +x_0=0 +y_0=0",              # nocov
                          " +a=6370000 +b=6370000",      # nocov
                          " +units=m +no_defs")          # nocov
  } else if(map_proj == 3){                              # nocov
    geogrd.proj <-paste0("+proj=merc +lat_ts=",truelat1, # nocov
                         " +lon_0=",ref_lon,             # nocov
                         " +a=6370000 +b=6370000",       # nocov
                         " +datum=WGS84")                # nocov
  } else if(map_proj %in% c(0, 6)){                      # nocov
    geogrd.proj <- paste0("+proj=eqc +lat_ts=",0,        # nocov
                          " +lat_0=",cen_lat,            # nocov
                          " +lon_0=",ref_lon,            # nocov
                          " +x_0=",0," +y_0=",0,         # nocov
                          " +ellps=WGS84 +units=m")      # nocov
  } else {
    stop('Error: Projection type not supported (currently Lambert Conformal, Cylindrical Equidistant, Polar and lat-lon WRF grids are suported).') # nocov
  }

  dx <- ncdf4::ncatt_get(coordNC, varid=0, attname="DX")$value
  dy <- ncdf4::ncatt_get(coordNC, varid=0, attname="DY")$value
  if ( dx != dy ) {
    stop(paste0('Error: Asymmetric grid cells not supported. DX=', dx, ', DY=', dy))  # nocov
  }

  nc_close(coordNC)

  # USING the terra R-package
  pontos     <- terra::vect(cbind(x[1], y[1]),
                            type = "points",
                            crs = "+proj=longlat +datum=WGS84 +no_defs")
  rm(x,y)
  transform  <- terra::project(pontos, geogrd.proj)
  projcoords <- terra::crds(transform)

  # coordinates here are the cell center,
  # We need to calculate the boundaries for the raster file
  xmn <- projcoords[1,1] - dx/2.0   # Left border
  ymx <- projcoords[1,2] + dy/2.0   # upper border
  xmx <- xmn + ncols*dx             # Right border
  ymn <- ymx - nrows*dy             # Bottom border

  f2 <- function(a, wh){
    dims <- seq_len(length(dim(a)))                # nocov
    dims <- setdiff(dims, wh)                      # nocov
    x    <- apply(apply(a, dims, rev), dims, t)    # nocov
    return(x)                                      # nocov
  }

  u <- ncdf4::ncatt_get(nc = wrf,varid = name, attname =  'units')$value

  ## processing by layer
  # make_rast <- function(i = 1, x = POL){
  #   if(verbose) cat('processing layer',i,'\n')
  #   x <- x[,,i,,drop = TRUE]
  #   # Create a multi-layer SpatRaster
  #   r <- terra::rast(resolution = dx,
  #                    xmin = xmn,
  #                    xmax = xmx,
  #                    ymin = ymn,
  #                    ymax = ymx,
  #                    nlyrs = dim(x)[3],
  #                    crs = geogrd.proj)
  #   r[]       <- c(f2(x,1))
  #   # names(r) <- paste(name,ncvar_get(wrf,'Times'),sep="_") ## need to test
  #   if(u != 0) units(r) <- u
  #   return(r)
  # }
  # r_list <- lapply(X = 1:dim(POL)[3],FUN = make_rast)

  ## processing by time
  make_rast <- function(i = 1, x = POL){
    # if(verbose) cat('processing time',i,'\n')
    x <- x[,,,i,drop = TRUE]
    # Create a multi-layer SpatRaster
    r <- terra::rast(resolution = dx,
                     xmin = xmn,
                     xmax = xmx,
                     ymin = ymn,
                     ymax = ymx,
                     nlyrs = dim(x)[3],
                     crs = geogrd.proj)
    r[]       <- c(f2(x,1))
    names(r)  <- paste(name,'level',formatC(1:dim(r)[3],width = 2, format = "d", flag = "0"),sep="_")
    if(u != 0) units(r) <- u
    if(flip_h) r <- terra::flip(r,direction='horizontal') # nocov
    if(flip_v) r <- terra::flip(r,direction='vertical')   # nocov
    return(r)
  }
  r_list <- lapply(X = 1:dim(POL)[4],FUN = make_rast)
  r      <- terra::sds(r_list)

  if('Times' %in% names(wrf$var)){
    TIME <- ncdf4::ncvar_get(wrf,'Times')
    TIME <- as.POSIXlt(TIME,
                       tz = "UTC",
                       format="%Y-%m-%d_%H:%M:%OS",
                       optional=FALSE)
    times <- list()
    for(i in 1:length(TIME)){
      times[[i]] <- rep(TIME[i],nlyr(r)[1])
    }
    terra::time(r) <- times
  }
  names(r) <- paste0(name,'_',1:length(TIME),'h')

  mem_order <- ncdf4::ncatt_get(nc = wrf,varid = name, attname = 'MemoryOrder')$value
  if(mem_order %in% c('XY','XY ')) r <- terra::flip(r,direction ='horizontal')        # nocov

  nc_close(wrf)

  if(latlon){
    return(terra::project(x = r,y = "+proj=longlat +datum=WGS84 +no_defs",method = method)) # nocov
  }else{
    return(r)
  }
}
