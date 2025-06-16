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

  avo        = 6.02E+23   # Avogadro s number
  R_ideal    = 8.314      # Ideal gas constant
  gas_cons   = 1E-6       # Conversion constant: ppm to mol/mol
  m2_cm2     = 1E4        # Conversion constant: m2 to cm2
  final_cons = avo / R_ideal * gas_cons / m2_cm2

  wrf <- ncdf4::nc_open(file)
  on.exit(nc_close(wrf))

  PHB  <- ncdf4::ncvar_get(wrf,'PHB', verbose = verbose)
  PH   <- ncdf4::ncvar_get(wrf,'PH',  verbose = verbose)
  P1   <- ncdf4::ncvar_get(wrf,'PB',  verbose = verbose)
  P2   <- ncdf4::ncvar_get(wrf,'P',   verbose = verbose)
  Temp <- ncdf4::ncvar_get(wrf,'T',   verbose = verbose)

  r    <- wrf_rast(file = file, name = name, verbose = TRUE)
  VAR  <- rast_to_netcdf(r)

  P    <- P1 + P2                        # total pressure [pa]
  Temp <- (Temp+300)*((P/10000)^0.286)   # temperature    [ K]
  dz   <- calculate_DZ(PHB,PH)           # calculate dz   [ m]

  VAR  <- final_cons * VAR * dz * (P/Temp) #  [molecules cm-2]

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

  if(DU){
    du  = 2.687E+16  # Conversion constant: molecules cm-2 to DU
    VAR <- VAR / du
    units(r) <- 'DU'
  }else{
    units(r) <- 'molecules cm-2'
  }

  return(r)
}
