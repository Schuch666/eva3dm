#' Function to convert/save a SpatRaster array/Netcdf
#'
#' @description Conversion of SpatRaster to array and optionally save on a existing Netcdf File.
#'
#' @param r SpatRaster object
#' @param file Netcdf file name
#' @param name variable name on a Netcdf file
#' @param unit unit of the variable (set to NA to don't change unit)
#' @param XY set to true if MemoryOrder is XY (only if file is missing)
#' @param verbose display additional information
#'
#' @return numerical array
#'
#' @import terra ncdf4
#'
#' @export
#'
#' @note eva3dm::wrf_rast support 3d SpatRaster, in case of a 4d variable use other approach to save on file.
#'
#' @examples
#' folder   <- system.file("extdata",package="eva3dm")
#' wrf_file <- paste0(folder,"/wrf.day1.o3.nc")
#'
#' Rast     <- wrf_rast(wrf_file,'o3')
#' A        <- rast_to_netcdf(Rast)
#'

rast_to_netcdf <- function(r,file,name, unit = units(r), XY = FALSE, verbose = TRUE){

  if(!class(r) %in% 'SpatRaster')
    stop('input is not a SpatRaster') # nocov

  if(missing(name))
    name <- deparse(substitute(r))

  if(missing(file) & XY)
    r <- terra::flip(r,direction='horizontal') # nocov

  if(nlyr(r) > 1 & time(r)[1] == time(r)[2]){
    revert = FALSE                             # nocov
  }else{
    revert = TRUE
  }

  N_times <- dim(r)[3]
  a       <- array(NA,c(dim(r)[2],dim(r)[1],N_times))
  if(N_times == 1){
    a[,,1]  <- as.matrix(rev(r[[1]]))
  }else{
    for(i in 1:N_times){                                           # nocov
      a[,,i]  <- as.matrix(flip(r[[i]], direction = 'horizontal')) # nocov
    }
    if(revert) a[,,1:N_times] <- a[,,N_times:1]                    # nocov
  }

  if(!missing(file) & !missing(name)){
    if(verbose) cat(paste0('writing ',name,' in ', file,'\n'))

    neto <- ncdf4::nc_open(filename = file, write = TRUE, suppress_dimvals = TRUE,return_on_error=TRUE)

    mem_order <- ncdf4::ncatt_get(nc = neto,varid = name, attname =  'MemoryOrder')
    if(mem_order$value %in% c('XY','XY ')) r <- terra::flip(r,direction='horizontal')

    ncdf4::ncvar_put(nc = neto,varid = name,vals = a)

    unit <- unit[1]
    if(!is.na(unit) & unit != ""){
      if(verbose) cat(paste0("units of ",name," set to \'",unit,"\' in ",file,"\n"))
      ncdf4::ncatt_put(nc = neto,varid = name,attname = 'units', attval = unit)
    }

    ncdf4::nc_close(neto)
  }else{
    return(a)
  }
}
