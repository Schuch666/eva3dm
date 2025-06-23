#' Calculate column concentration of trace gases form WRF-Chem
#'
#' @description Read output from WRF model and calculate the column of trace gases.
#'
#' @param file WRF output file, see notes
#' @param name trace gas name to be integrated
#' @param met (optional) WRF output file for meteorological variables, see notes
#' @param DU true to change the output units from 'molecules cm-1' to 'DU'
#' @param flip_v passed to wrf_rast, see notes
#' @param flip_h passed to wrf_rast, see notes
#' @param verbose display additional information
#' @param ... extra arguments passed to eva3dm::wrf_rast or eva3dm::wrf_sds
#'
#' @return SpatRaster object (from terra package).
#'
#' @note files in file should contain Times, XLAT, XLONG variables in addition to the concentration to be integrated, the variable should include at least 3 dimensions including vertical.
#'
#' @note met is a optional file, it should have containing PHB, PH, PB, P, and T variables with the same dimension of the concentration integrated.
#'
#' @note post processing can affect the orientation of the variables, the arguments flip_v and flip_h and other arguments from eva3dm::wrf_rast and eva3dm::wrf_sds can be used to take effect into account.
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
                             flip_v = FALSE,
                             flip_h = FALSE,
                             verbose = TRUE,
                             ...){

  wrf <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(wrf))

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
      dz     <- array(0,c(dim(Z)[1],dim(Z)[2],dim(Z)[3]-1,dim(Z)[4]))
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

  f4 <- function(x){
    x <- as.array(x)
    x <- aperm(x, c(2,1,3,4))
    x <- x[rev(seq_len(dim(x)[1])),,,]
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

  if(length(dim(Temp)) == 3){
    r    <- wrf_rast(file    = file,
                     name    = name,
                     flip_v  = flip_v,
                     flip_h  = flip_v,
                     verbose = verbose, ... )
    VAR  <- rast_to_netcdf(r)
  }else if(length(dim(Temp)) == 4){
    sds <- wrf_sds(file    = file,
                   name    = name,
                   flip_v  = flip_v,
                   flip_h  = flip_v,
                   verbose = verbose, ... )
    VAR <- f4(sds)
  }else if(length(dim(Temp)) <= 2){
    stop('insuficient meteorological input')  # less data than needed / include more data
  }else{
    stop('meteorological input not suported') # dimension not supported / reduce data
  }

  P    <- P1 + P2                        # total pressure [pa]
  Temp <- (Temp+300)*((P/100000)^0.286)  # temperature    [ K]
  dz   <- calculate_DZ(PHB,PH)           # calculate dz   [ m]

  VAR  = final_cons * VAR * (P/Temp) * dz  #  [molecules cm-2]

  if(length(dim(VAR)) == 3){               #  XLAT XLONG LEVEL
    r[]      <- c(f3(VAR,1))
    times_r  <- time(r)
    r        <- sum(r, na.rm = TRUE)
    names(r) <- paste0(name,'_column')
    time(r)  <- times_r[1]
  }
  if(length(dim(VAR)) == 4){               #  XLAT XLONG LEVEL TIME
    times_r   <- time(sds[,1])
    make_rast <- function(i = 1, x = sds){
      r   <- sds[[i,]]
      r[] <- c(f3(VAR[,,,i],1))
      r   <- sum(r, na.rm = TRUE)
      names(r) <- paste0(name,'_column')
      return(r)
    }
    r_list   <- lapply(X = 1:length(times_r),FUN = make_rast)
    r        <- terra::rast(r_list)
    time(r)  <- times_r
  }

  if(DU){
    du  = 2.687E+16  # Conversion constant: molecules cm-2 to DU
    r   <- r / du
    units(r) <- 'DU'
  }else{
    units(r) <- 'molecules cm-2'
  }
  return(r)
}
