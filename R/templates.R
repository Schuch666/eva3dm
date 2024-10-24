#' Create templates for model evaluation
#'
#' @description Create templates of code (r-scripts and job submission script) to read, post-process and evaluate model results.
#'
#' @param root directory to create the template
#' @param template template type (see notes)
#' @param case case to be evaluated
#' @param env name of the conda environment
#' @param scheduler scheduler name
#' @param partition name of the partition
#' @param project project name
#' @param verbose display additional information
#'
#' @export
#'
#' @note templates available:\cr
#'  - WRF (model post-process for METAR + INMET)\cr
#'  - WRF-Chem (model post-process for METAR, AQS in Brazil and AERONET)\cr
#'  - IPEN (model post-process for one experimental site)\cr
#'  - METAR (download observations)\cr
#'  - MET (evaluation of meteorology)\cr
#'  - AQ (evaluation of air quality)\cr
#'  - GPCP (evaluation of precipitation using satellite)
#'
#' @examples
#' temp <- file.path(tempdir(),"POST")
#' template(root = temp,template = 'WRF', case = 'WRF-only')
#'

template <- function(root      = getwd(),
                     template  = 'WRF',
                     case      = 'case',
                     env       = 'rspatial',
                     scheduler = 'SBATCH',
                     partition = 'main',
                     project   = 'PROJECT',
                     verbose   = TRUE){

  ### SETUP RUNNING FOLDER
  if(root != '')
    root = paste0(root,'/')

  ### SETUP HEADERS
  if(scheduler == 'SBATCH'){
    HEADER <- paste0('#!/bin/bash --login
#SBATCH -J R-Post
#SBATCH -N 1
#SBATCH -n 20
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
#PBS -l select=1:ncpus=20:mpiprocs=20')
  }

  ### SETUP of METEOROLOGY POST
  if(template == 'WRF'){
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
Rscript extract_inmet.R $dir P       4d &
Rscript extract_inmet.R $dir SWDOWN  3d &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
rm metar.d0* inmet.d0*
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
   ndim <- "4d"
}

if(ndim == "&")
   ndim <- "4d"

sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_METAR.Rds"))

files <- dir(path = paste0("WRF/",dir),
             pattern = "wrfout_d01",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "metar.d01")

  ',
  file = paste0(root,'extract_metar.R'),
  append = F)

    cat('args <- commandArgs(trailingOnly = TRUE)

library(eva3dm)

dir  <- args[1]

var  <- args[2]
if(length(args) > 2){
   ndim <- args[3]
}else{
   ndim <- "4d"
}

if(ndim == "&")
   ndim <- "4d"

sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_INMET.Rds"))

files <- dir(path = paste0("WRF/",dir),
             pattern = "wrfout_d01",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "inmet.d01")

  ',
file = paste0(root,'extract_inmet.R'),
append = F)

    if(verbose)
      cat(' folder ',paste0(root,'WRF/',case),': link wrf output files here!
 bash ',   paste0(root,'post-R.sh'),': post processing job script
 r-script',paste0(root,'extract_metar.R'),': source code to extract metar using eva3dm::extract_serie()
 r-script',paste0(root,'extract_inpet.R'),': source code to extract inmet using eva3dm::extract_serie()\n')
  }

### SETUP of CHEMESTRY AND METEOROLOGY
if(template == 'WRF-Chem'){
  dir.create(path = paste0(root,'WRF/',case),
             recursive = T,
             showWarnings = F)

  cat(paste0(HEADER,'

dir=\'',case,'\'

cd ',root,'

conda activate ',env,'

echo \'folder:\' $dir

date

echo \'extracting METAR time-series ...\'

Rscript extract_metar.R $dir T2     3d &
Rscript extract_metar.R $dir P      4d &
Rscript extract_metar.R $dir Q2     3d &
Rscript extract_metar.R $dir U10    3d &
Rscript extract_metar.R $dir V10    3d &
Rscript extract_metar.R $dir RAINC  3d &
Rscript extract_metar.R $dir RAINNC 3d &

echo \'extracting AQ time-series for meteorology ...\'
Rscript extract_aq.R $dir T2 3d  &
Rscript extract_aq.R $dir Q2 3d  &
Rscript extract_aq.R $dir P      &
Rscript extract_aq.R $dir U10 3d &
Rscript extract_aq.R $dir V10 3d &

echo \'extracting AQ time-series for species ...\'
Rscript extract_aq.R $dir o3        &
Rscript extract_aq.R $dir no        &
Rscript extract_aq.R $dir no2       &
Rscript extract_aq.R $dir co        &
Rscript extract_aq.R $dir so2       &
Rscript extract_qa.R $dir nh3       &
Rscript extract_aq.R $dir PM2_5_DRY &
Rscript extract_aq.R $dir PM10      &

wait
echo \'extracting pm composition part 1...\'

Rscript extract_pm.R $dir so4aj     &
Rscript extract_pm.R $dir nh4aj     &
Rscript extract_pm.R $dir no3aj     &
Rscript extract_pm.R $dir naaj      &
Rscript extract_pm.R $dir claj      &
Rscript extract_pm.R $dir orgpaj    &
Rscript extract_pm.R $dir ecj       &
Rscript extract_pm.R $dir p25j      &
Rscript extract_pm.R $dir asoa1j    &
Rscript extract_pm.R $dir asoa2j    &
Rscript extract_pm.R $dir asoa3j    &
Rscript extract_pm.R $dir asoa4j    &
Rscript extract_pm.R $dir bsoa1j    &
Rscript extract_pm.R $dir bsoa2j    &
Rscript extract_pm.R $dir bsoa3j    &
Rscript extract_pm.R $dir bsoa4j    &
Rscript extract_pm.R $dir so4ai     &
Rscript extract_pm.R $dir nh4ai     &
Rscript extract_pm.R $dir no3ai     &
Rscript extract_pm.R $dir naai      &

wait
echo \'extracting pm composition part 2...\'

Rscript extract_pm.R $dir clai      &
Rscript extract_pm.R $dir orgpai    &
Rscript extract_pm.R $dir eci       &
Rscript extract_pm.R $dir p25i      &
Rscript extract_pm.R $dir asoa1i    &
Rscript extract_pm.R $dir asoa2i    &
Rscript extract_pm.R $dir asoa3i    &
Rscript extract_pm.R $dir asoa4i    &
Rscript extract_pm.R $dir bsoa1i    &
Rscript extract_pm.R $dir bsoa2i    &
Rscript extract_pm.R $dir bsoa3i    &
Rscript extract_pm.R $dir bsoa4i    &
Rscript extract_pm.R $dir antha     &
Rscript extract_pm.R $dir soila     &
Rscript extract_pm.R $dir seas      &
Rscript extract_pm.R $dir TOT_DUST  &
Rscript extract_pm.R $dir DMS_0     &
Rscript extract_pm.R $dir TSOA      &
Rscript extract_pm.R $dir BSOA      &
Rscript extract_pm.R $dir ASOA      &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
rm metar.d0* aq.d0* pm.d0*
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
   ndim <- "4d"
}

if(ndim == "&")
   ndim <- "4d"

sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_METAR.Rds"))

files <- dir(path = paste0("WRF/",dir),
             pattern = "wrfout_d01",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "metar.d01")

  ',
file = paste0(root,'extract_metar.R'),
append = F)

  cat('args <- commandArgs(trailingOnly = TRUE)

library(eva3dm)

dir  <- args[1]

var  <- args[2]
if(length(args) > 2){
   ndim <- args[3]
}else{
   ndim <- "4d"
}

if(ndim == "&")
   ndim <- "4d"

sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))

files <- dir(path = paste0("WRF/",dir),
             pattern = "wrfout_d01",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "aq.d01")

  ',
file = paste0(root,'extract_aq.R'),
append = F)

  cat('args <- commandArgs(trailingOnly = TRUE)

library(eva3dm)

dir  <- args[1]

var  <- args[2]
if(length(args) > 2){
   ndim <- args[3]
}else{
   ndim <- "4d"
}

if(ndim == "&")
   ndim <- "4d"

sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AERONET.Rds"))

files <- dir(path = paste0("WRF/",dir),
             pattern = "wrfout_d01",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "pm.d01")

  ',
file = paste0(root,'extract_pm.R'),
append = F)

  if(verbose)
    cat(' folder ',paste0(root,'WRF/',case),': link wrf output files here!
 bash ',   paste0(root,'post-R.sh'),': post processing job script
 r-script',paste0(root,'extract_metar.R'),': source code to extract metar using eva3dm::extract_serie()
 r-script',paste0(root,'extract_aq.R'),': source code to extract AQ stations from Brazil using eva3dm::extract_serie()
 r-script',paste0(root,'extract_pm.R'),': source code to extract PM compositon from AERONET sites using eva3dm::extract_serie()\n')
}

}
