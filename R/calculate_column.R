#' Calculate column concentration of trace gases form WRF-Chem
#'
#' @description Read output from WRF model and calculate the column of trace gases.
#'
#' @param file WRF output file
#' @param name trace gas name
#' @param met (optional) WRF output file for meteorological variables
#' @param verbose display additional information
#' @param ... extra arguments passed to ncdf4::ncvar_get (eva3dm::wrf_rast and eva3dm::wrf_sds)
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
                             met,
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
    # POL   <- eva3dm::wrf_rast(file = wrf,name = names(wrf$var)[name], ... ) ## eva3dm::wrf_sds
    name  <- names(wrf$var)[name]                                     # nocov end
  }else{
    POL   <- ncdf4::ncvar_get(wrf,name, ... )
    # POL   <- eva3dm::wrf_rast(file = wrf,name = name, ... ) ## eva3dm::wrf_sds
  }
  if(verbose) cat(paste0('integrating vertically ',name,' from ', file,'\n')) # nocov

  # if(missing(map)){                                                           # nocov
  #   coord_file = file                                                         # nocov
  # }else{                                                                      # nocov
  #   coord_file = met                                                          # nocov
  #   if(verbose) cat('using meterological variables from',met,'file\n')        # nocov
  # }

  return()
}
