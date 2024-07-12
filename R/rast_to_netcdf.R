#' Function to convert/save a SpatRaster array/Netcdf
#'
#' @description Conversion of SpatRaster to array and opcionally save on a Netcdf File.
#'
#' @param r SpatRaster object
#' @param file Netcdf file name
#' @param name variable name on a Netcdf file
#' @param verbose display additional information
#'
#' @import terra ncdf4
#'
#' @export
#'
#' @examples
#' folder   <- system.file("extdata",package="eval3dmodel")
#' wrf_file <- paste0(folder,"/wrf.day1.o3.nc")
#'
#' Rast     <- wrf_rast(wrf_file,'o3')
#' A        <- rast_to_netcdf(Rast)
#'

rast_to_netcdf <- function(r,file,name, verbose = TRUE){
  if(!class(r) %in% 'SpatRaster')
    stop('input is not a SpatRaster')

  N_times <- dim(r)[3]
  a       <- array(NA,c(dim(r)[1],dim(r)[2],N_times))
  if(N_times == 1){
    a[,,1]  <- as.matrix(rev(r[[1]]))
  }else{
    for(i in 1:N_times){
      a[,,i]  <- as.matrix(flip(r[[i]], direction = 'vertical'))
    }
    a[,,1:N_times] <- a[,,N_times:1]
  }

  if(!missing(file) & !missing(name)){
    if(verbose) cat(paste0('writing ',name,' on ', file,'\n'))
    nc <- ncdf4::nc_open(filename = file, write = TRUE)
    ncvar_put(nc = nc,varid = name,vals = a)
    ncdf4::nc_close(nc)
  }else{
    return(a)
  }
}
