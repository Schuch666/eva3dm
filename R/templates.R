#' Create templates for meodel evaluation
#'
#' @description Calculate traditional statistics from two numerical vectors in related to a threshold
#'
#' @param root directory to create the template
#' @param template template type
#' @param case case to be evaluated
#' @param env name of the conda environment
#' @param scheduler scheduler name
#' @param partition name of the partition
#' @param project project name
#' @param verbose display additional information
#'
#' @export
#'
#' @examples
#' temp <- file.path(tempdir(),"POST")
#' template(root = temp)
#'

template <- function(root      = getwd(),
                     template  = 'WRF-MET',
                     case      = 'WRF-only',
                     env       = 'rspatial',
                     scheduler = 'SBATCH',
                     partition = 'main',
                     project   = 'PROJECT',
                     verbose   = TRUE){

  if(root != '')
    root = paste0(root,'/')

  if(scheduler == 'SBATCH'){
    HEADER <- paste0('#!/bin/bash --login
#SBATCH -J R-Post
#SBATCH -N 1
#SBATCH -n 15
#SBATCH --time=12:00:00
#SBATCH -p ',partition,'
#SBATCH --mem=0
#SBATCH --exclusive')
  }

  if(scheduler == "PBS"){
    HEADER <- paste0('#!/bin/bash --login
#PBS -N post-R
#PBS -A ',project,'
#PBS -l walltime=12:00:00
#PBS -q ',partition,'
#PBS -j oe
#PBS -m abe
#PBS -M email@gmail.com
### ex Select 3 nodes with 48 CPUs each for a total of 144 MPI processes
#PBS -l select=1:ncpus=15:mpiprocs=15')
  }

  if(template == 'WRF-MET'){
    dir.create(path = paste0(root,'WRF/',case),
               recursive = T,
               showWarnings = F)

    cat(paste0(HEADER,'

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

echo \'extracting INMET time-series...\'

Rscript extract_inmet.R $dir T2      3d &
Rscript extract_inmet.R $dir Q2      3d &
Rscript extract_inmet.R $dir U10     3d &
Rscript extract_inmet.R $dir V10     3d &
Rscript extract_inmet.R $dir RAINNC  3d &
Rscript extract_inmet.R $dir RAINC   3d &
Rscript extract_inmet.R $dir P          &
Rscript extract_inmet.R $dir SWDOWN  3d &

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
      cat(' folder ',paste0(root,'WRF/',case),': link wrf output files here!
 bash ',   paste0(root,'post-R.sh'),': post processing job script
 r-script',paste0(root,'extract_metar.R'),': source code to extract metar using eva3dm::extract_serie()\n')
  }

}

# dir.create(path = paste0(root,'METAR'),
#            recursive = T,
#            showWarnings = F)
