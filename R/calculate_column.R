#' Calculate column concentration of trace gases form WRF-Chem
#'
#' @description Read output from WRF model and calculate the column of trace gases.
#'
#' @param file WRF output file
#' @param name trace gas name
#' @param met (optional) WRF output file for meteorological variables
#' @param DU true to change the units from 'molecules cm-1' to 'DU'
#' @param verbose display additional information
#' @param ... extra arguments passed to ncdf4::ncvar_get
#'
#' @return SpatRaster object (from terra package).
#'
#' @import terra ncdf4
#' @importFrom utils menu
#'
#' @export
#'

calculate_column <- function(file = file.choose(),
                             name = NA,
                             met = file,
                             DU = FALSE,
                             verbose = FALSE,
                             ...){

  # if(missing(name)){
  #   if(name == 'time'){
  #     wrf <- ncdf4::nc_open(file)                                                       # nocov
  #     if(verbose)                                                                       # nocov
  #       cat(paste0('reading Times from ', file,'\n'))                                   # nocov
  #     TIME <- ncdf4::ncvar_get(wrf,'Times')                                             # nocov
  #     TIME <- as.POSIXlt(TIME, tz = "UTC", format="%Y-%m-%d_%H:%M:%OS", optional=FALSE) # nocov
  #     if(verbose)                                                                       # nocov
  #       cat('returning Times in POSIXct\n')                                             # nocov
  #     return(TIME)                                                                      # nocov
  #   }
  # }

  # wrf <- ncdf4::nc_open(file)
  #
  # if(is.na(name)){                                                  # nocov start
  #   name  <- menu(names(wrf$var), title = "Choose the variable:")
  #   POL   <- ncdf4::ncvar_get(wrf, names(wrf$var)[name], ... )
  #   name  <- names(wrf$var)[name]                                   # nocov end
  # }else{
  #   POL   <- ncdf4::ncvar_get(wrf,name, ... )
  # }
  # if(verbose) cat(paste0('reading ',name,' from ', file,'\n'))      # nocov
  # if(verbose) cat(paste("creating SpatRaster for",name,'\n'))       # nocov

  # if(missing(met)){                                                           # nocov
  #   met = file                                                                # nocov
  #   if(verbose) cat('using meterological variables from',met,'file\n')        # nocov
  # }

  calculate_DZ <- function(PHB,PH, g = 9.817){
    Z      <- (PHB+PH)/g
    if(length(dim(Z)) == 3){                  # this is a version for 3d (XYZ)
      dz     <- array(0,c(dim(Z)[1],dim(Z)[2],dim(Z)[3]-1))
      for(i in 1:(dim(Z)[3]-1)){
        dz[,,i] = Z[,,i+1] - Z[,,i]
      }
    }else if(length(dim(Z)) == 4){            # this is a version for 4d (XYZT)
      dz     <- array(0,c(dim(Z)[1],dim(Z)[2],dim(Z)[3]-1), dim(Z)[4])
      for(i in 1:(dim(Z)[3]-1)){
        dz[,,i,] = Z[,,i+1,] - Z[,,i,]
      }
    }else{
      stop('dimention not suported')
    }
    return(dz)
  }

  avo     = 6.02E+23
  rfac    = 8.314
  dobfac  = 2.687E+16
  fac2    = avo/rfac * 1E-4 * 1E-6

  wrf <- ncdf4::nc_open(file)
  on.exit(nc_close(wrf))

  PHB  <- ncdf4::ncvar_get(wrf,'PHB', verbose = verbose)
  PH   <- ncdf4::ncvar_get(wrf,'PH',  verbose = verbose)
  P1   <- ncdf4::ncvar_get(wrf,'PB',  verbose = verbose)
  P2   <- ncdf4::ncvar_get(wrf,'P',   verbose = verbose)
  Temp <- ncdf4::ncvar_get(wrf,'T',   verbose = verbose)

  # PHB  <- eixport::wrf_get(file = met, name = 'PHB', verbose = verbose)
  # PH   <- eixport::wrf_get(file = met, name = 'PH',  verbose = verbose)
  # P1   <- eixport::wrf_get(file = met, name = 'PB',  verbose = verbose)
  # P2   <- eixport::wrf_get(file = met, name = 'P',   verbose = verbose)
  # Temp <- eixport::wrf_get(file = met, name = 'T',   verbose = verbose)
  # # VAR  <- eixport::wrf_get(file = file, name = 'o3',  verbose = TRUE)

  r    <- wrf_rast(file = file, name = name, verbose = TRUE)
  VAR  <- rast_to_netcdf(r)

  P    <- P1 + P2
  Temp <- (Temp+300)*((P/10000)^0.286)
  dz   <- calculate_DZ(PHB,PH)

  VAR  <- fac2 * VAR * dz * (P/Temp)

  if(is.matrix(VAR)){
    r[]       <- rev(c(VAR))
  }else if(length(dim(VAR)) == 3){
    f2 <- function(a, wh){
      dims <- seq_len(length(dim(a)))                # nocov
      dims <- setdiff(dims, wh)                      # nocov
      x    <- apply(apply(a, dims, rev), dims, t)    # nocov
      return(x)                                      # nocov
    }
    r[]       <- c(f2(VAR,1))
  }
  r <- sum(r, na.rm = TRUE)

  # if(DU){
  #   VAR <- VAR / dobfac
  #   cat('DU\n')
  #   # units
  # }else{
  #   cat('molecules cm-1\n')
  #   # units
  # }

  return(r)
}
