#' Read and write attributes on a NetCDF file
#'
#' @description Read and write metadata information of a NetCDF files
#'
#' @param file file name
#' @param var variable name, 0 to global and "?" to show options
#' @param att attribute names (NA for get all attnames)
#' @param action "na" (only print), "write" or "get" (return the value) of an attribute
#' @param value value to write
#' @param verbose display additional information
#'
#' @import ncdf4
#'
#' @export
#'
#' @examples
#' nc <- paste0(system.file("extdata",package="eva3dm"),'/wrfinput_d01')
#' atr(nc,0)
#' atr(nc,'Times')
#' atr(nc,'XLAT')
#' atr(nc,'XLONG')
#'
#' atr(nc,'XLONG','MemoryOrder')
#' atr(nc,'XLONG','description')
#' atr(nc,'XLONG','units')
#' atr(nc,'XLONG','stagger')
#' atr(nc,'XLONG','FieldType')
#'

atr <- function(file = NA,var = '?', att = NA, action="na", value=NA, verbose=T){

  meta <- NULL
  on.exit(ncdf4::nc_close(meta))

  if(is.na(file[1])){
    cat("choose a file:\n") # nocov
    file <- file.choose()   # nocov
    cat(paste(file,"\n"))   # nocov
  }
  if(!action %in% c("na","get"))
    to_write <- T
  else
    to_write <- F

  meta <- ncdf4::nc_open(filename = file, write = to_write)

  if(var == "?"){
    name  <- menu(names(meta$var), title = "Choose the variable:")
    POL   <- ncdf4::ncvar_get(meta, names(meta$var)[name])
    var   <- names(meta$var)[name]
  }

  if(action == "na" | action == "get"){
    if(is.na(att)){
      ATR <- ncdf4::ncatt_get(meta,var,att)
      if(var==0){
        if(verbose) cat("global attributes:\n")
      }else{
        if(verbose) cat(paste0("variable ",var," attritutes:\n"))
      }
      if(verbose){
        if(is.null(names(ATR))){
          cat("not found\n")
        }
        else{
          cat(paste(names(ATR),sep = ","))
        }
      }
    }else{
      ATR <- ncdf4::ncatt_get(meta,var,att)
      if(var == 0)
        var <- "global"
      if(verbose)  cat(paste0(var," attribute ",att,":\n"))
      if(verbose){
        if(is.null(ATR$value)){
          cat("not found\n")
        }else{
          if(ATR$value == ""){
            cat("empty\n")
          }else{
            cat(paste0(ATR$value,"\n"))
          }
        }
      }
    }
    if(action == 'get') return(ATR$value)
  }else{
    if(is.na(value))
      stop("nothing to write") # nocov
    if(var == 0){
      if(verbose)
        cat(paste0("writing \'",value,"\' on global attribute \'",att,"\'\nat file ",file,"\n"))
    }else{
      if(verbose)
        cat(paste0("writing \'",value,"\' on attribute \'",att," \' of ",var,"\nat file ",file,"\n"))
    }

    ncdf4::ncatt_put(meta,varid = var,attname = att,attval = value)
  }
}
