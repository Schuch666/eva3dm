#' Calculate column concentration of trace gases form WRF-Chem
#'
#' @description Read output from WRF model and calculate the column of trace gases.
#' The column concentration \eqn{ C_{\mathrm{column}}} is computed as:
#'
#' \deqn{ C_{\mathrm{column}} = \frac{N_{A}}{R} \int_{\text{surface}}^{\text{model\_top}} C \frac{P}{T} \, dz }
#'
#' where \eqn{C} is the pollutant concentration, \eqn{N_{A}} is Avogadro's number, \eqn{R} is the universal gas constant, \eqn{P} is the pressure [Pa], \eqn{T} is the temperature [K], and \eqn{dz} is the layer thickness [m].
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
#' @examples
#' file <- paste0(system.file("extdata",package="eva3dm"),"/wrf_column_o3_Boston.nc")
#' O3_column <- calculate_column(file,'o3', verbose = TRUE)
#'
#' @export
#'

calculate_column <- function(file = file.choose(),
                             name,
                             met,
                             DU,
                             flip_v = FALSE,
                             flip_h = FALSE,
                             verbose = TRUE,
                             ...){

  wrf <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(wrf))

  if(missing(met)){        # nocov start
    met     <- file
    wrf_met <- wrf
  }else{
    wrf_met <- ncdf4::nc_open(met)
    on.exit(nc_close(wrf_met))
  }

  if(missing(name)){
    n    <- menu(names(wrf$var), title = "Choose the variable:")
    name <- names(wrf$var)[n]
  }

  if(missing(DU)){
    if(name == 'o3'){
      if(verbose)
        cat('output unit set to DU for o3\n')
      DU = TRUE
    }else{
      DU = FALSE
    }
  }                       # nocov end

  calculate_DZ <- function(PHB,PH, g = 9.817){
    Z      <- (PHB+PH)/g
    if(length(dim(Z)) == 3){                  # this is a version for 3d (XYZ)
      dz     <- array(0,c(dim(Z)[1],dim(Z)[2],dim(Z)[3]-1))
      for(i in 1:(dim(Z)[3]-1)){
        dz[,,i] = Z[,,i+1] - Z[,,i]
      }
    }else if(length(dim(Z)) == 4){            # this is a version for 4d (XYZT) # nocov
      dz     <- array(0,c(dim(Z)[1],dim(Z)[2],dim(Z)[3]-1,dim(Z)[4]))           # nocov
      for(i in 1:(dim(Z)[3]-1)){                                                # nocov
        dz[,,i,] = Z[,,i+1,] - Z[,,i,]                                          # nocov
      }
    }else{
      stop('dimention not suported') # nocov
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
    x <- as.array(x)                      # nocov
    x <- aperm(x, c(2,1,3,4))             # nocov
    x <- x[rev(seq_len(dim(x)[1])),,,]    # nocov
    return(x)                             # nocov
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
  }else if(length(dim(Temp)) == 4){         # nocov
    sds <- wrf_sds(file    = file,          # nocov
                   name    = name,          # nocov
                   flip_v  = flip_v,        # nocov
                   flip_h  = flip_v,        # nocov
                   verbose = verbose, ... ) # nocov
    VAR <- f4(sds)                          # nocov
  }else if(length(dim(Temp)) <= 2){         # nocov
    stop('insuficient meteorological input')  # less data than needed / include more data # nocov
  }else{
    stop('meteorological input not suported') # dimension not supported / reduce data # nocov
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
  if(length(dim(VAR)) == 4){               #  XLAT XLONG LEVEL TIME   # nocov
    times_r   <- time(sds[,1])                                        # nocov
    make_rast <- function(i = 1, x = sds){                            # nocov
      r   <- sds[[i,]]                                                # nocov
      r[] <- c(f3(VAR[,,,i],1))                                       # nocov
      r   <- sum(r, na.rm = TRUE)                                     # nocov
      names(r) <- paste0(name,'_column')                              # nocov
      return(r)                                                       # nocov
    }                                                                 # nocov
    r_list   <- lapply(X = 1:length(times_r),FUN = make_rast)         # nocov
    r        <- terra::rast(r_list)                                   # nocov
    time(r)  <- times_r                                               # nocov
  }

  if(DU){
    du  = 2.687E+16  # Conversion constant: molecules cm-2 to DU
    r   <- r / du
    units(r) <- 'DU'
  }else{
    units(r) <- 'molecules cm-2' # nocov
  }
  return(r)
}
