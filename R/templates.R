#' Create templates for meodel evaluation
#'
#' @description Calculate traditional statistics from two numerical vectors in related to a threshold
#'
#' @param template template name
#' @param case case to be evaluated
#' @param partition name of the partition
#' @param env name of the conda environment
#' @param root directory to create the template
#' @param verbose display additional information
#'
#' @export
#'
#' @examples
#' temp <- file.path(tempdir(),"POST")
#' template(root = temp)
#'

template <- function(template  = 'WRF-METAR',
                     case      = 'WRF-only',
                     partition = 'zhang',
                     env       = 'rspatial',
                     root      = getwd(),
                     verbose   = TRUE){

  if(root != '')
    root = paste0(root,'/')

  if(template == 'WRF-METAR'){
    dir.create(path = paste0(root,'WRF/',case),
               recursive = T,
               showWarnings = F)
    dir.create(path = paste0(root,'METAR'),
               recursive = T,
               showWarnings = F)

    cat(paste0('#!/bin/bash --login
#SBATCH -J R-Post
#SBATCH -N 1
#SBATCH -n 7
#SBATCH --time=12:00:00
#SBATCH -p ',partition,'
#SBATCH --mem=0
#SBATCH --exclusive

dir=\'',case,'\'

cd ',root,'

conda activate ',env,'

echo \'folder:\' $dir

date

echo \'extracting METAR time-series...\'

Rscript extract_metar.R $dir T2     3d &
Rscript extract_metar.R $dir P      4d &
Rscript extract_metar.R $dir Q2     3d &
Rscript extract_metar.R $dir U10    3d &
Rscript extract_metar.R $dir V10    3d &
Rscript extract_metar.R $dir RAINC  3d &
Rscript extract_metar.R $dir RAINNC 3d &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
rm metar.d0*
'),
file = paste0(root,'post-R.sh'),
append = F)

    cat('args <- commandArgs(trailingOnly = TRUE)

library(eva3dm)

dir  <- args[1]

var  <- args[2]
if(length(args) > 2){
   ndim <- args[3]
}else{
   ndim <- \'4d\'
}

if(ndim == \'&\')
   ndim <- \'4d\'

sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),
                        "/sites_METAR.Rds"))

files    <- dir(path = paste0(\'WRF/\',dir),
                pattern = "wrfout_d01",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "metar.d01",
              fast     = TRUE)

  ',
  file = paste0(root,'extract_metar.R'),
  append = F)
    if(verbose)
      cat(' folder ',paste0(root,'WRF/',case),': link wrf output files
 folder',paste0(root,'METAR'),': download metar
 bash ',   paste0(root,'post-R.sh'),': post processing job script
 r-script',paste0(root,'extract_metar.R'),': source code to extract metar using eva3dm\n')
  }
}
