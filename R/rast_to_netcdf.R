#' Function to convert/save a SpatRaster array/Netcdf
#'
#' @description Conversion of SpatRaster to array and opcionally save on a Netcdf File.
#'
#' @param r SpatRaster object
#' @param file Netcdf file name
#' @param name variable name on a Netcdf file
#' @param unit unit of the variable (set to NA to don't change unit)
#' @param verbose display additional information
#'
#' @import terra ncdf4
#'
#' @export
#'
#' @examples
#' folder   <- system.file("extdata",package="eva3dm")
#' wrf_file <- paste0(folder,"/wrf.day1.o3.nc")
#'
#' Rast     <- wrf_rast(wrf_file,'o3')
#' A        <- rast_to_netcdf(Rast)
#'

rast_to_netcdf <- function(r,file,name, unit = units(r),verbose = TRUE){

  if(!class(r) %in% 'SpatRaster')
    stop('input is not a SpatRaster') # nocov

  if(!missing(file) & !missing(name)){
    if(verbose) cat(paste0('writing ',name,' on ', file,'\n')) # nocov

    nc <- ncdf4::nc_open(filename = file, write = TRUE)        # nocov

    mem_order <- ncdf4::ncatt_get(nc = nc,varid = name, attname =  'MemoryOrder')                 # nocov
    if(mem_order$value %in% c('XY','XY ')) r <- terra::flip(r,direction='horizontal')             # nocov

    N_times <- dim(r)[3] # nocov
    a       <- array(NA,c(dim(r)[1],dim(r)[2],N_times)) # nocov
    if(N_times == 1){ # nocov
      a[,,1]  <- as.matrix(rev(r[[1]])) # nocov
    }else{
      for(i in 1:N_times){ # nocov
        a[,,i]  <- as.matrix(flip(r[[i]], direction = 'vertical')) # nocov
      }
      a[,,1:N_times] <- a[,,N_times:1]                             # nocov
    }
    ncvar_put(nc = nc,varid = name,vals = a)                       # nocov
    nc_close(nc)
    unit <- unit[1]
    if(!is.na(unit) & unit != "")                              # novoc
      atr(file = file,var = name,                              # nocov
          att = 'units',action = 'write',                      # nocov
          value = unit,verbose = verbose)                      # nocov
  }else{
    N_times <- dim(r)[3]
    a       <- array(NA,c(dim(r)[1],dim(r)[2],N_times))
    if(N_times == 1){
      a[,,1]  <- as.matrix(rev(r[[1]]))
    }else{
      for(i in 1:N_times){                                         # nocov
        a[,,i]  <- as.matrix(flip(r[[i]], direction = 'vertical')) # nocov
      }
      a[,,1:N_times] <- a[,,N_times:1]                             # nocov
    }
    return(a)
  }
}
