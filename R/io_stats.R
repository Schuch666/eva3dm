#' Functions to write stats and evaluation
#'
#' @description Functions to write stats and evaluation output. If a file name ending with .csv
#' is provided to the function will save using write.csv otherwise the function write.table.
#'
#' @param stat observed data.frame
#' @param file model data.frame
#' @param sep the field separator string, passed to write.table function
#' @param dec he string to use for decimal points, passed to write.table function
#' @param ... arguments passed to write.table and write.csv functions
#' @param verbose display additional information
#'
#' @importFrom utils read.csv write.csv read.table write.table
#'
#' @export
#'
#' @examples
#'
#' sample <- read_stat(paste0(system.file("extdata", package = "eva3dm"),"/sample.csv"),
#'                     verbose = TRUE)
#' dir.create(file.path(tempdir(), "stats"))
#'
#' write_stat(file    = paste0(file.path(tempdir(), "stats"),'/sample.txt'),
#'            stat    = sample,
#'            verbose = TRUE)
#'
#' write_stat(file    = paste0(file.path(tempdir(), "stats"),'/sample.csv'),
#'            stat    = sample,
#'            verbose = TRUE)
#'
write_stat <- function(stat,file, sep = ';',dec = '.', verbose = FALSE, ...){
  if(verbose)
    cat('writing', file,'\n')
  if(substr(file,nchar(file)-3,nchar(file)) == '.csv'){
    write.csv(x = stat, file = file, ...)
  }else{
    write.table(x = stat, file = file, sep = sep, dec = dec, ...)
  }
}

#' Function to read stats and evaluation
#'
#' @description Function to read stats and evaluation output
#'
#' @param file model data.frame
#' @param sep the field separator string, passed to read.table function
#' @param dec he string to use for decimal points, passed to read.table function
#' @param ... arguments passed to read.table functions
#' @param verbose display additional information
#'
#' @export
#'
#' @examples
#' sample <- read_stat(file    = paste0(system.file("extdata", package = "eva3dm"),"/sample.txt"),
#'                     verbose = TRUE)
#'
#' sample <- read_stat(file    = paste0(system.file("extdata", package = "eva3dm"),"/sample.csv"),
#'                     verbose = TRUE)
#'
read_stat <- function(file, sep = ';',dec = '.',verbose = FALSE, ...){
  if(verbose)
    cat('reading', file,'\n')
  if(substr(file,nchar(file)-3,nchar(file)) == '.csv'){
    stat            <- read.csv(file = file)
    row.names(stat) <- stat$X
    stat            <- stat[,-1]
    names(stat) <- gsub("....", " (%)", names(stat), fixed = T)
  }else{
    stat <- read.table(file = file, sep = sep, dec = dec, ...)
    names(stat) <- gsub("....", " (%)", names(stat), fixed = T)
  }
  return(stat)
}
