#' Read and write attributes on a NetCDF file
#'
#' @description Read and write metadata information of a NetCDF files
#'
#' @param file file name
#' @param var variable name, 0 to global and "?" to show options
#' @param att attribute names (NA for get all attnames)
#' @param action Print, write or get (return the value) an attribute
#' @param value value to write
#' @param verbose display additional information
#'
#' @import ncdf4
#'
#' @export
#'
#' @examples
#' nc <- paste0(system.file("extdata",package="eval3dmodel"),'/wrfinput_d01')
#' atr(nc,0, action = 'print')
#' atr(nc,'Times', action = 'print')
#' atr(nc,'XLAT', action = 'print')
#' atr(nc,'XLONG', action = 'print')
#'
#' atr(nc,'XLONG','MemoryOrder', action = 'print')
#' atr(nc,'XLONG','description', action = 'print')
#' atr(nc,'XLONG','units', action = 'print')
#' atr(nc,'XLONG','stagger', action = 'print')
#' atr(nc,'XLONG','FieldType', action = 'print')
#'

atr <- function(file = NA,var = '?', att = NA, action="get", value=NA, verbose=F){

  meta <- NULL
  on.exit(ncdf4::nc_close(meta))

  if(is.na(file[1])){
    cat("choose a file:\n") # nocov
    file <- file.choose()   # nocov
    cat(paste(file,"\n"))   # nocov
  }
  if(action != "read")
    to_write <- T
  else
    to_write <- F

  meta <- ncdf4::nc_open(filename = file, verbose = verbose, write = to_write)

  if(var == "?"){
    name  <- menu(names(meta$var), title = "Choose the variable:")
    POL   <- ncdf4::ncvar_get(meta, names(meta$var)[name])
    var   <- names(meta$var)[name]
  }

  if(action == "print" | action == "get"){
    if(is.na(att)){
      ATR <- ncdf4::ncatt_get(meta,var,att,verbose=verbose)
      if(var==0)
        cat("global attributes:\n")
      else
        cat(paste0("variable ",var," attritutes:\n"))
      cat(paste(names(ATR),sep = ","))
    }else{
      ATR <- ncdf4::ncatt_get(meta,var,att,verbose=verbose)
      if(var == 0)
        var <- "global"
      cat(paste0(var," attribute ",att,":\n"))
      cat(paste0(ATR$value,"\n"))
    }
    if(action == 'get') return(ATR)
  }else{
    if(is.na(value))
      stop("nothing to write") # nocov
    if(var == 0){
      cat(paste0("writing \'",value,"\' on global attribute \'",att,"\'\nat file ",file,"\n"))
    }else{
      cat(paste0("writing \'",value,"\' on attribute \'",att," \' of ",var,"\nat file ",file,"\n"))
    }

    ncdf4::ncatt_put(meta,varid = var,attname = att,attval = value,verbose = verbose)
  }
}
