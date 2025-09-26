#' Extract time series from a list model files using time, latitude, longitude, and altitude.
#'
#' @description Read output from WRF model and calculate the column of trace gases.
#'
#' @param filelist list of files to be read
#' @param point data.frame with time (POSIXct), lat, lon, alt
#' @param vars variable names
#' @param model 'WRF' (default) or 'UFS'
#' @param include_indices to include t,i,j,k indices
#' @param include_model to include model time, latitude, longitude and altitude
#' @param include_distances include the distance from the center of the grid to the observations
#' @param tol tolerance in seconds
#' @param boundary number of grid points that are considered
#' @param filename name of a file to save the dataframe
#' @param verbose display additional information
#'
#' @return data.frame
#'
#' @import ncdf4
#'
#' @export
#'

extract_surgical <- function(filelist,
                             point,
                             vars,
                             model             = 'WRF',
                             tol               = 60,
                             boundary          = 5,
                             include_indices   = TRUE,
                             include_model     = TRUE,
                             include_distances = TRUE,
                             filename,
                             verbose           = TRUE) {
  results <- list()

  for (f in filelist) {
    cat('opening', f, '\n')
    nc <- nc_open(f)

    # --- TIME HANDLING ---
    if(model == 'UFS'){
      time_var    <- ncvar_get(nc, "time")
      time_units  <- ncatt_get(nc, "time", "units")$value
      ref_time    <- sub("hours since ", "", time_units)
      times_posix <- as.POSIXct(ref_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") + time_var * 3600
    } else if(model == 'WRF'){
      times_posix <- wrf_rast(f,"time", verbose = FALSE)
    } else{
      stop(paste(model,'model not supported!'))
    }

    match_idx <- c()
    for (p in seq_len(nrow(point))) {
      diffs <- abs(difftime(times_posix, point$time[p], units = "secs"))
      if (min(diffs) <= tol) {
        match_idx <- c(match_idx, p)
      }
    }
    if (length(match_idx) == 0) {
      nc_close(nc)
      next
    }

    # --- LAT / LON / Z HANDLING ---
    if(model == 'UFS'){
      lat         <- ncvar_get(nc,'lat')
      lon         <- ncvar_get(nc,'lon')
      z           <- calculate_UFS_z(nc)
    } else if(model == 'WRF'){
      lat         <- ncvar_get(nc,'XLAT')
      lon         <- ncvar_get(nc,'XLONG')
      z           <- calculate_WRF_z(nc)
      if (length(dim(lat)) == 3) {
        lat <- lat[,,1]
        lon <- lon[,,1]
      } else if (length(dim(lat)) == 4) {
        lat <- lat[,,1,1]
        lon <- lon[,,1,1]
      }
    }

    for (tt in match_idx) {
      t  <- point$time[tt]
      la <- point$lat[tt]
      lo <- point$lon[tt]
      al <- point$alt[tt]

      # Nearest time
      tidx <- which.min(abs(times_posix - t))

      # Horizontal distance
      dist_h <- matrix(NA, nrow = nrow(lat), ncol = ncol(lat))
      for (jj in 1:nrow(lat)) {
        for (ii in 1:ncol(lat)) {
          dist_h[jj, ii] <- eva3dm::get_distances(la, lo, lat[jj, ii], lon[jj, ii])
        }
      }
      idx  <- which(dist_h == min(dist_h), arr.ind = TRUE)
      i    <- as.numeric(idx[1,1])
      j    <- as.numeric(idx[1,2])

      base_row <- list(
        time   = t,
        lon    = lo,
        lat    = la,
        alt    = al
      )

      fill_outside <- function(x,
                               include_indices   = include_indices,
                               include_model     = include_model,
                               include_distances = include_distances){
        if (include_indices) {
          x$t <- NA
          x$i <- NA
          x$j <- NA
          x$k <- NA
        }
        if(include_model){
          x$model <- NA
          x$x     <- NA
          x$y     <- NA
          x$z     <- NA
        }
        if(include_distances){
          x$h_dist <- dist_h[i, j]
          x$v_diff <- (zz[i, j, k] - al)/1000
          x$t_dist <- sqrt(dist_h[i, j]^2 + zz[i, j, k]^2)/1000
        }
        return(x)
      }

      inside_4_walls = TRUE
      if (     i <= boundary)             inside_4_walls = FALSE
      else if (i >= nrow(lat) - boundary) inside_4_walls = FALSE
      else if (j <= boundary)             inside_4_walls = FALSE
      else if (j >= ncol(lat) - boundary) inside_4_walls = FALSE

      if(verbose & !inside_4_walls){
        cat('point outside the domain or close to boundary by',boundary,'points or less!\n')
      }

      if(length(dim(z)) == 4){
        zz = z[,,,tidx]
      }else{
        zz = z
      }
      k    <- which.min(abs(zz[i,j,] - al))

      base_row <- list(
        time   = t,
        lon    = lo,
        lat    = la,
        alt    = al
      )
      if (include_indices) {
        base_row$t <- tidx
        base_row$i <- i
        base_row$j <- j
        base_row$k <- k
      }
      if(include_model){
        base_row$model <- times_posix[tidx]
        base_row$x     <- lon[i, j]
        base_row$y     <- lat[i, j]
        base_row$z     <- zz[i, j, k]
      }
      if(include_distances){
        base_row$h_dist <- dist_h[i, j]
        base_row$v_diff <- (zz[i, j, k] - al)/1000
        base_row$t_dist <- sqrt(dist_h[i, j]^2 + zz[i, j, k]^2)/1000
      }

      # --- Extract variables dynamically ---
      var_values <- list()
      for (v in vars) {
        if (v %in% names(nc$var) & inside_4_walls) {
          vals <- ncvar_get(nc, v)
          nd   <- length(dim(vals))

          if (nd == 4) {       # (lon, lat, lev, time)
            var_values[[v]] <- vals[i, j, k, tidx]
          } else if (nd == 3) { # maybe (lon, lat, lev)
            var_values[[v]] <- vals[i, j, k]
          } else if (nd == 2) { # (lon, lat)
            var_values[[v]] <- vals[i, j]
          } else {
            var_values[[v]] <- NA
          }
        } else {
          var_values[[v]] <- NA
        }
      }

      # --- Combine results ---
      results[[length(results) + 1]] <- data.frame(
        base_row,
        as.data.frame(var_values)
      )
    }

    nc_close(nc)
  }

  final <- do.call(rbind, results)
  if(!missing(filename)){
    if(verbose) cat('salving',filename,'\n')
    saveRDS(final,filename)
  }
  return(final)
}


calculate_WRF_z <- function(wrf, reference = "altitude") {
  PHB <- ncdf4::ncvar_get(wrf, varid = "PHB")
  PH  <- ncdf4::ncvar_get(wrf, varid = "PH")

  # Get terrain height or set to zero based on reference
  if (reference == "sea_level") {
    HGT <- 0  # Zero out terrain for sea level reference
  } else if (reference == "altitude") {
    HGT <- ncdf4::ncvar_get(wrf, varid = "HGT")  # Use terrain for AGL
  } else {
    stop("reference must be either 'altitude' or 'sea_level'")
  }

  time_length <- ifelse("Time" %in% names(wrf$dim), wrf$dim$Time$len,  1)

  z <- PH

  if (time_length == 1) {
    for (i in 1:dim(PH)[3]) {
      z[,,i] <- (PH[,,i] + PHB[,,i]) / 9.8 - HGT
    }
  } else {
    for (i in 1:dim(PH)[3]) {
      z[,,i,] <- (PH[,,i,] + PHB[,,i,]) / 9.8 - HGT
    }
  }

  return(z)
}

library(ncdf4)

calculate_UFS_z <- function(nc, verbose = TRUE) {
  Rd <- 287.04   # Gas constant for dry air (J/kg/K)
  g <- 9.80665   # Gravity (m/s^2)

  # if(verbose) cat('opening',file,'...\n')
  # nc <- nc_open(file)

  ak   <- ncatt_get(nc, 0, "ak")[[2]]
  bk   <- ncatt_get(nc, 0, "bk")[[2]]
  ps   <- ncvar_get(nc, "pressfc")
  temp <- ncvar_get(nc, "tmp")

  nlon <- dim(temp)[1]
  nlat <- dim(temp)[2]
  nlev <- length(ak)
  nlev_temp <- nlev - 1

  if ("hgtsfc" %in% names(nc$var)) {
    zsfc <- ncvar_get(nc, "hgtsfc")
  } else if ("orog" %in% names(nc$var)) {
    zsfc <- ncvar_get(nc, "orog")
  } else {
    zsfc <- array(0, dim = c(nlon, nlat))
  }

  # nc_close(nc)

  if(verbose) cat('calculating pressure and z ...\n')

  pressure <- array(NA, dim = c(nlon, nlat, nlev))
  for (k in 1:nlev) {
    pressure[,,k] <- ak[k] + bk[k] * ps
  }

  pressure_mid <- array(NA, dim = c(nlon, nlat, nlev_temp))
  for (k in 1:nlev_temp) {
    pressure_mid[,,k] <- (pressure[,,k] + pressure[,,k+1]) / 2
  }

  height_mid <- array(NA, dim = c(nlon, nlat, nlev_temp))
  height_mid[,,nlev_temp] <- zsfc

  for (k in (nlev_temp-1):1) {
    Tavg <- (temp[,,k] + temp[,,k+1]) / 2
    p_lower         <- pressure_mid[,,k+1]
    p_upper         <- pressure_mid[,,k]
    dz              <- (Rd * Tavg / g) * log(p_lower / p_upper)
    height_mid[,,k] <- height_mid[,,k+1] + dz
  }

  return(height_mid)
}
