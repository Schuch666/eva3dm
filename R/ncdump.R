#' Print a 'ncdump -h' command
#'
#' @description Read a NetCDF and print the medatada
#'
#' @param file file name
#'
#' @return No return value, only display information
#'
#' @import ncdf4
#'
#' @export
#'
#' @examples
#' ncdump(file = paste0(system.file("extdata",package="eva3dm"),
#'                      '/wrfinput_d01'))
#'

ncdump <- function(file = file.choose()){
  n <- NULL
  on.exit(ncdf4::nc_close(n))
  n <- ncdf4::nc_open(file)
  print(n)
}
