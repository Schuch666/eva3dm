#' Function to return variable names
#'
#' @description Return variable names of a NetCDF
#'
#' @param file file name
#' @param action 'get' to return variable names or 'print' to print
#' @param verbose display additional information
#'
#' @import ncdf4
#'
#' @export
#'
#' @examples
#' vars(paste0(system.file("extdata",package="NH4"),'/wrfinput_d01'))
#'

vars <- function(file = NA, action="get", verbose=F){
  meta <- NULL
  on.exit(ncdf4::nc_close(meta))

  if(is.na(file[1])){
    cat("choose a file:\n") # nocov
    file <- file.choose()   # nocov
    cat(paste(file,"\n"))   # nocov
  }

  meta <- ncdf4::nc_open(filename = file, verbose = verbose, write = F)

  if(action == 'print'){
    return(cat(names(meta$var),'\n'))
  }else{
    return(names(meta$var))
  }
}
