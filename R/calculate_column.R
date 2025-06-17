#' Calculate column concentration of trace gases form WRF-Chem
#'
#' @description Read output from WRF model and calculate the column of trace gases.
#'
#' @param file WRF output file, see notes
#' @param name trace gas name to be integrated
#' @param met (optional) WRF output file for meteorological variables, see notes
#' @param DU true to change the output units from 'molecules cm-1' to 'DU'
#' @param verbose display additional information
#' @param ... extra arguments passed to ncdf4::ncvar_get
#'
#' @return SpatRaster object (from terra package).
#'
#' @note files in file should contain Times, XLAT, XLONG variables in addition to the concentration to be integrated, the variable should include at least 3 dimensions including vertical.
#'
#' @note met is a optional file, it should have containing PHB, PH, PB, P, and T variables with the same dimension of the concentration integrated.
#'
#' @import terra ncdf4
#' @importFrom utils menu
#'
#' @export
#'

calculate_column <- function(file = file.choose(),
                             name,
                             met,
                             DU = FALSE,
                             verbose = TRUE,
                             ...){

  wrf <- ncdf4::nc_open(file)
  on.exit(nc_close(wrf))

  if(missing(met)){
    met     <- file
    wrf_met <- wrf
  }else{
    wrf_met <- ncdf4::nc_open(met)
    on.exit(nc_close(wrf_met))
  }

  if(missing(name)){                                               # nocov start
    n    <- menu(names(wrf$var), title = "Choose the variable:")
    name <- names(wrf$var)[n]                                      # nocov end
  }

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

  f3 <- function(a, wh){
    dims <- seq_len(length(dim(a)))
    dims <- setdiff(dims, wh)
    x    <- apply(apply(a, dims, rev), dims, t)
    return(x)
  }

  avo        = 6.02E+23   # Avogadro s number
  R_ideal    = 8.314      # Ideal gas constant
  gas_cons   = 1E-6       # Conversion constant: ppm to mol/mol
  m2_cm2     = 1E4        # Conversion constant: m2 to cm2
  final_cons = avo / R_ideal * gas_cons / m2_cm2

  if(verbose) cat(paste0('reading meteorology from ', met,'\n'))
  PHB  <- ncdf4::ncvar_get(wrf_met,'PHB', verbose = FALSE)
  PH   <- ncdf4::ncvar_get(wrf_met,'PH',  verbose = FALSE)
  P1   <- ncdf4::ncvar_get(wrf_met,'PB',  verbose = FALSE)
  P2   <- ncdf4::ncvar_get(wrf_met,'P',   verbose = FALSE)
  Temp <- ncdf4::ncvar_get(wrf_met,'T',   verbose = FALSE)

  if(verbose) cat(paste0('reading ',name,' from ', file,'\n'))
  r    <- wrf_rast(file = file, name = name, verbose = FALSE)
  VAR  <- rast_to_netcdf(r)

  P    <- P1 + P2                        # total pressure [pa]
  Temp <- (Temp+300)*((P/100000)^0.286)  # temperature    [ K]
  dz   <- calculate_DZ(PHB,PH)           # calculate dz   [ m]

  VAR  = final_cons * VAR * (P/Temp) * dz  #  [molecules cm-2]

  if(length(dim(VAR)) == 3){               #  XLAT XLONG LEVEL
    r[]       <- c(f3(VAR,1))
  }
  times_r  <- time(r)
  r        <- sum(r, na.rm = TRUE)
  names(r) <- paste0(name,'_column')
  time(r)  <- times_r[1]

  if(DU){
    du  = 2.687E+16  # Conversion constant: molecules cm-2 to DU
    r   <- r / du
    units(r) <- 'DU'
  }else{
    units(r) <- 'molecules cm-2'
  }

  return(r)
}
