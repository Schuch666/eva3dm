#' Create templates for model evaluation
#'
#' @description Create templates of code (r-scripts and bash job-submission script) to read, post-process and evaluate model results.
#'
#' @param root directory to create the template
#' @param template template type (see notes)
#' @param case case to be evaluated
#' @param env name of the conda environment
#' @param scheduler job scheduler used (SBATCH or PBS)
#' @param partition partition name
#' @param project project name
#' @param verbose display additional information
#'
#' @return no value returned, create folders and other template scripts
#'
#' @export
#'
#' @note Templates types available:\cr
#'  - WRF (model post-process for METAR + INMET)\cr
#'  - WRF-3 (model post-process for METAR + INMET for triple nested domains)\cr
#'  - WRF-Chem (model post-process for METAR, AQS in Brazil and AERONET)\cr
#'  - EXP (model post-process for one experimental site including PBL variables)\cr
#'  - CAMx (post-process for triple tested domains)
#'  - METAR (download METAR and other meteorological observations from ASOS)\cr
#'  - MET (evaluation of meteorology)\cr
#'  - MET-3 (evaluation of meteorology for triple nested domains)\cr
#'  - AQ (evaluation of air quality)\cr
#'  - PSA (model post-processing with CDO for satellite evaluation)\cr
#'  - SAT (evaluation of precipitation using GPCP satellite)\cr
#'  - AQS_BR (download data from air quality stations at Sao Paulo and Rio de Janeiro)\cr
#'  - INMET (pre-processing of automatic and conventional meteorological data from INMET)\cr
#'  - merge (merge INMET data and merge METAR data)\cr
#'  - ISD (process METAR and other meteorological data from ISD)\cr
#'
#' @examples
#' temp <- file.path(tempdir(),"POST")
#' template(root = temp,template = 'WRF', case = 'WRF-only')
#'

template <- function(root,
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
#SBATCH -n 22
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
#PBS -l select=1:ncpus=22:mpiprocs=22')
  }

  ### SETUP of METEOROLOGY POST
  if(template == 'WRF'){
    dir.create(path = paste0(root,'WRF/',case),
               recursive = TRUE,
               showWarnings = FALSE)

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
mv metar.d0* inmet.d0* WRF/$dir
'),
file = paste0(root,'post-R_wrf.sh'),
append = FALSE)

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
  append = FALSE)

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
append = FALSE)

    if(verbose)
      cat(' folder ',paste0(root,'WRF/',case),': link wrf output files here!
 bash ',   paste0(root,'post-R_wrf.sh'),': post processing job script
 r-script',paste0(root,'extract_metar.R'),': source code to extract metar using eva3dm::extract_serie()
 r-script',paste0(root,'extract_inmet.R'),': source code to extract inmet using eva3dm::extract_serie()\n')
  }

### SETUP of METEOROLOGY POST for 3 domains
if(template == 'WRF-3'){
  dir.create(path = paste0(root,'WRF/',case),
             recursive = TRUE,
             showWarnings = FALSE)

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

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
mv metar.d0* inmet.d0* WRF/$dir
'),
      file = paste0(root,'post-R_wrf.sh'),
      append = FALSE)

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

files <- dir(path = paste0("WRF/",dir),
             pattern = "wrfout_d02",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "metar.d02")

files <- dir(path = paste0("WRF/",dir),
             pattern = "wrfout_d03",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "metar.d03")

  ',
file = paste0(root,'extract_metar.R'),
append = FALSE)

  if(verbose)
    cat(' folder ',paste0(root,'WRF/',case),': link wrf output files here!
 bash ',   paste0(root,'post-R_wrf.sh'),': post processing job script
 r-script',paste0(root,'extract_metar.R'),': source code to extract metar using eva3dm::extract_serie()\n')
}


### SETUP of CHEMESTRY AND METEOROLOGY
if(template == 'WRF-Chem'){
  dir.create(path = paste0(root,'WRF/',case),
             recursive = TRUE,
             showWarnings = FALSE)

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
mv metar.d0* aq.d0* pm.d0* WRF/$dir
'),
      file = paste0(root,'post-R_wrfchem.sh'),
      append = FALSE)

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
append = FALSE)

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
append = FALSE)

  if(verbose)
    cat(' folder ',paste0(root,'WRF/',case),': link wrf output files here!
 bash ',   paste0(root,'post-R_wrfchem.sh'),': post processing job script
 r-script',paste0(root,'extract_metar.R'),': source code to extract metar using eva3dm::extract_serie()
 r-script',paste0(root,'extract_aq.R'),': source code to extract AQ stations from Brazil using eva3dm::extract_serie()
 r-script',paste0(root,'extract_pm.R'),': source code to extract PM compositon from AERONET sites using eva3dm::extract_serie()\n')
}


### SETUP for an experiment (PM composition MET / CHEM and PBL variables)
if(template == 'EXP'){
  dir.create(path = paste0(root,'WRF/',case),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0(HEADER,'

dir=\'',case,'\'

cd ',root,'

conda activate ',env,'

echo \'folder:\' $dir

date

echo \'extracting AQ time-series for meteorology ...\'
Rscript extract_exp.R $dir T2 3d  &
Rscript extract_exp.R $dir Q2 3d  &
Rscript extract_exp.R $dir P      &
Rscript extract_exp.R $dir U10 3d &
Rscript extract_exp.R $dir V10 3d &

echo \'extracting AQ time-series for species ...\'
Rscript extract_exp.R $dir o3        &
Rscript extract_exp.R $dir no        &
Rscript extract_exp.R $dir no2       &
Rscript extract_exp.R $dir co        &
Rscript extract_exp.R $dir so2       &
Rscript extract_exp.R $dir nh3       &
Rscript extract_exp.R $dir PM2_5_DRY &
Rscript extract_exp.R $dir PM10      &

wait
echo \'extracting pm composition part 1...\'

Rscript extract_exp.R $dir so4aj     &
Rscript extract_exp.R $dir nh4aj     &
Rscript extract_exp.R $dir no3aj     &
Rscript extract_exp.R $dir naaj      &
Rscript extract_exp.R $dir claj      &
Rscript extract_exp.R $dir orgpaj    &
Rscript extract_exp.R $dir ecj       &
Rscript extract_exp.R $dir p25j      &
Rscript extract_exp.R $dir asoa1j    &
Rscript extract_exp.R $dir asoa2j    &
Rscript extract_exp.R $dir asoa3j    &
Rscript extract_exp.R $dir asoa4j    &
Rscript extract_exp.R $dir bsoa1j    &
Rscript extract_exp.R $dir bsoa2j    &
Rscript extract_exp.R $dir bsoa3j    &
Rscript extract_exp.R $dir bsoa4j    &
Rscript extract_exp.R $dir so4ai     &
Rscript extract_exp.R $dir nh4ai     &
Rscript extract_exp.R $dir no3ai     &
Rscript extract_exp.R $dir naai      &

wait
echo \'extracting pm composition part 2...\'

Rscript extract_exp.R $dir clai      &
Rscript extract_exp.R $dir orgpai    &
Rscript extract_exp.R $dir eci       &
Rscript extract_exp.R $dir p25i      &
Rscript extract_exp.R $dir asoa1i    &
Rscript extract_exp.R $dir asoa2i    &
Rscript extract_exp.R $dir asoa3i    &
Rscript extract_exp.R $dir asoa4i    &
Rscript extract_exp.R $dir bsoa1i    &
Rscript extract_exp.R $dir bsoa2i    &
Rscript extract_exp.R $dir bsoa3i    &
Rscript extract_exp.R $dir bsoa4i    &
Rscript extract_exp.R $dir antha     &
Rscript extract_exp.R $dir soila     &
Rscript extract_exp.R $dir seas      &
Rscript extract_exp.R $dir TOT_DUST  &
Rscript extract_exp.R $dir DMS_0     &
Rscript extract_exp.R $dir TSOA      &
Rscript extract_exp.R $dir BSOA      &
Rscript extract_exp.R $dir ASOA      &

wait
echo \'extracting PBL variables...\'

Rscript extract_exp.R $dir PBLH   3d &
Rscript extract_exp.R $dir LH     3d &
Rscript extract_exp.R $dir HFX    3d &
Rscript extract_exp.R $dir QFX    3d &
Rscript extract_exp.R $dir UST    3d &
Rscript extract_exp.R $dir RAINNC 3d &
Rscript extract_exp.R $dir RAINC  3d &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
mv exp.d0* WRF/$dir
'),
      file = paste0(root,'post-R_exp.sh'),
      append = FALSE)

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

# experimental site at Cidade Universitaria-USP, Sao Paulo-BR
site <- data.frame(name = "Ipen",
                   lat = -23.56634,
                   lon = -46.73741,
                   source = "IAG-USP",
                   stringsAsFactors = F)

row.names(site) <- site$name
site$name       <- "Cidade Universitaria - USP"

files <- dir(path = paste0("WRF/",dir),
             pattern = "wrfout_d01",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = site,
              variable = var,
              field    = ndim,
              prefix   = "exp.d01")

  ',
file = paste0(root,'extract_exp.R'),
append = FALSE)

  if(verbose)
    cat(' folder ',paste0(root,'WRF/',case),': link wrf output files here!
 bash ',   paste0(root,'post-R_exp.sh'),': post processing job script
 r-script',paste0(root,'extract_expr.R'),': source code to extract metar using eva3dm::extract_serie()
 new locations can be added to the data.frame with the list of sites.\n')
}


### SCRIPT TO DOWNLOAD METAR
if(template == 'METAR'){
  dir.create(path = paste0(root,'METAR'),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0('library("eva3dm")
library("riem")

setwd("',root,'")

# set a folder to save the data start / end dates
root_folder <- "METAR"
start_date  <- "2023-01-01"
end_date    <- "2023-12-31"

# load the list of all sites
all_sites  <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_METAR.Rds"))

# extract i and j of points inside the domain of a wrfinput_d01 file
sites      <- extract_serie(filelist       = "wrfinput_d01",
                            point          = all_sites,
                            return.nearest = T)

for(site in row.names(sites)){
  cat("downloading METAR from:",site,grep(site,row.names(sites)),"of",length(row.names(sites)),"...\\n")

  DATA <- riem_measures(station    = site,
                        date_start = start_date,
                        date_end   = end_date,,
                        latlon     = TRUE)
  if(is.null(DATA)){
    cat(\'skiping\', site,\'\\n\')
    next
  }

  DATA <- as.data.frame(DATA)

  DATA2 <- data.frame(date    = DATA$valid,           # original date
                      station = DATA$station,         # station code
                      lon     = DATA$lon,             # longitude
                      lat     = DATA$lat,             # latitude
                      T2      = 5/9 * (DATA$tmpf-32), # Fahrenheit to Celcius
                      TD      = 5/9 * (DATA$dwpf-32), # Fahrenheit to Celcius
                      feel    = 5/9 * (DATA$feel-32), # Fahrenheit to Celcius
                      RH      = DATA$relh,            # relative humidity
                      WS      = 0.514444 * DATA$sknt, # Knots to m/s
                      WD      = DATA$drct,            # wind direction degrees N
                      P       = DATA$mslp,            # pressure
                      rain    = DATA$p01i)            # precipitation

  DATA2 <- hourly(DATA2)

  saveRDS(object = DATA2,file = paste0(root_folder,"/METAR.",site,".Rds"))
  # write.csv(x = DATA2,file = paste0(root_folder,"/METAR.",site,".csv"))
}

cat("download completed!")

  '),
file = paste0(root,'download_METAR.R'),
append = FALSE)

  if(verbose)
    cat(' folder ',paste0(root),': copy wrfinput_d01 here!
 folder ',paste0(root,'METAR'),': destination folder
 r-script',paste0(root,'download_METAR.R'),': script that download metar data using riem package and information from eva3dm and wrfinput_d01 file\n')
}

### SCRIPT TO EVALUATION USING METAR
if(template == 'MET'){

  dir.create(path = paste0(root,"WRF/",case),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0('library(eva3dm)

setwd("',root,'")

WRF_folder   = "WRF"
METAR_folder = "METAR"
case         = "',case,'"

min_WS       =  0.52 # use 0.5 // 1 Knots ~ 0.514444 m/s

sink(file = paste0(WRF_folder,case,"/eval.log"),split = T)

source("table_metar_T2.R")
source("table_metar_Q2.R")
source("table_metar_WS.R")
source("table_metar_WD.R")

cat("CASE:",case,"DONE!\\n")
sink()

  '),
file = paste0(root,'all_tables.R'),
append = FALSE)


  cat(paste0('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "',case,'"

model_d01     <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.T2.Rds"))
model_d01[-1] <- model_d01[-1] - 273.15 # to convert to Celcius

cat("opening TEMP:\\n")
files_obs <- dir(path = METAR_folder,pattern = ".Rds",full.names = T)
obs       <- data.frame(date = model_d01$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("open",files_obs[i],i,"of",length(files_obs),"\\n")
  new <- readRDS(files_obs[i])
  if(nrow(new) > 1 & !all(is.na(new$T2)) ){
    name       <- new$name[1]
    new        <- data.frame(date = new$date,
                             obs  = new$T2)
    cat("station name:",name,"\\n")
    names(new) <- c("date",name) # renaming T2 for the station name
    new        <- new[!duplicated(new$date), ]
    obs        <- merge(obs, new, by = "date",all.x = T,sort = TRUE)
  }else{
    cat("no data selected in:",files_obs[i],"\\n")
  }
}
observed <- obs

# observed[-1] <- observed[-1] + 273.15 # Celcius to Kelvin

cat("Temperature for d01:\\n")

mod_stats_d01 <- data.frame()

for(i in names(model_d01)[-1]){
  mod_stats_d01 <- eva(mo = model_d01,
                       ob = observed,
                       table = mod_stats_d01,
                       site  = i)
}
mod_stats_d01   <- mod_stats_d01[mod_stats_d01$n > 1, ]  # remove missing data
mod_stats_d01   <- eva(model_d01,observed,table = mod_stats_d01)
cat("...\\n")
print(tail(mod_stats_d01))
cat("\\n")

write_stat(stat = mod_stats_d01,
           file = paste0(WRF_folder,"/",case,"/stats.metar.T2.d01.csv"))


  '),
      file = paste0(root,'table_metar_T2.R'),
      append = FALSE)

  cat(paste0('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "',case,'"

model_d01        <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.Q2.Rds"))

cat("opening Q2:\\n")
files_obs <- dir(path = METAR_folder,pattern = ".Rds",full.names = T)
obs       <- data.frame(date = model_d01$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("open",files_obs[i],i,"of",length(files_obs),"\\n")
  new <- readRDS(files_obs[i])
  if(nrow(new) > 1 & !all(is.na(new$Q2)) ){
    name       <- new$name[1]
    new        <- data.frame(date = new$date,
                             obs  = new$Q2)

    cat("station name:",name,"\\n")
    names(new) <- c("date",name)    # renaming Q2 for the station name
    new        <- new[!duplicated(new$date), ]
    obs        <- merge(obs, new, by = "date",all.x = T,sort = TRUE)
  }else{
    cat("no data selected in:",files_obs[i],"\\n")
  }
}
observed <- obs

cat("convert units to kg/g\\n")
observed[-1]  <- observed[-1]  * 1000
model_d01[-1] <- model_d01[-1] * 1000

cat("remove obs > 40 kg/g from obs\\n")
DATA            <- observed[-1]
DATA[DATA > 40] <- NA
observed[-1]    <- DATA
rm(DATA)

# using daily average
model_d01 <- daily(data = model_d01)
observed  <- daily(data = observed)

cat("Q2 for d01:\\n")

mod_stats_d01 <- data.frame()

for(i in names(model_d01)[-1]){
  mod_stats_d01 <- eva(mo = model_d01,
                       ob = observed,
                       table = mod_stats_d01,
                       site  = i)
}
mod_stats_d01   <- mod_stats_d01[mod_stats_d01$n > 1, ]  # remove stations w/no data
mod_stats_d01   <- eva(model_d01,observed,"ALL",table = mod_stats_d01)
cat("...\\n")
print(tail(mod_stats_d01))
cat("\\n")

write_stat(stat = mod_stats_d01,
           file = paste0(WRF_folder,"/",case,"/stats.metar.Q2.d01.csv"))

  '),
file = paste0(root,'table_metar_Q2.R'),
append = FALSE)

  cat(paste0('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "case"

if(file.exists(paste0(WRF_folder,"/",case,"/metar.d01.WS.Rds"))){
  cat("opening calculated wind speed...\\n")
  model_d01_WS <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.WS.Rds"))
}else{
  U10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.U10.Rds"))
  V10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.V10.Rds"))
  model_d01_WS   <- uv2ws(u = U10, v = V10)
  saveRDS(model_d01_WS,paste0(WRF_folder,"/",case,"/metar.d01.WS.Rds"))
}

cat("opening WS:\\n")
files_obs <- dir(path = METAR_folder,pattern = ".Rds",full.names = T)
obs       <- data.frame(date = model_d01_WS$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("open",files_obs[i],i,"of",length(files_obs),"\\n")
  new <- readRDS(files_obs[i])
  if(nrow(new) > 1 & !all(is.na(new$WS)) ){
    name       <- new$name[1]
    new        <- data.frame(date = new$date,
                             obs  = new$WS)

    cat("station name:",name,"\\n")
    names(new) <- c("date",name)    # renaming WS for the station name
    new        <- new[!duplicated(new$date), ]
    obs        <- merge(obs, new, by = "date",all.x = T,sort = TRUE)
  }else{
    cat("no data selected in:",files_obs[i],"\\n")
  }
}
observed_ws <- obs

if(!is.na(min_WS)){
  cat("removing WS less than",min_WS,"\n")
  DATA                 <- observed[-1]
  DATA[DATA < min_WS ] <- NA
  observed[-1]         <- DATA
  rm(DATA)
}else{
  cat("removing WS less than zero\n")
  DATA              <- observed[-1]
  DATA[DATA < 0.0 ] <- NA
  observed[-1]      <- DATA
  rm(DATA)
}

cat("WS for d01:\\n")

mod_stats_d01_ws <- data.frame()

for(i in names(model_d01_WS)[-1]){
  mod_stats_d01_ws <- eva(mo = model_d01_WS,
                          ob = observed_ws,
                          table = mod_stats_d01_ws,
                          site  = i)
}
mod_stats_d01_ws   <- mod_stats_d01_ws[mod_stats_d01_ws$n > 1, ]  # remove stations w/no data
mod_stats_d01_ws   <- eva(model_d01_WS,observed_ws,"ALL",table = mod_stats_d01_ws)
cat("...\\n")
print(tail(mod_stats_d01_ws))
cat("\\n")

write_stat(stat = mod_stats_d01_ws,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WS.d01.csv"))
  '),
      file = paste0(root,'table_metar_WS.R'),
      append = FALSE)

  cat(paste0('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "case"

if(file.exists(paste0(WRF_folder,"/",case,"/metar.d01.WD.Rds"))){
  cat("opening calculated wind speed...\\n")
  model_d01_WD <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.WD.Rds"))
}else{
  U10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.U10.Rds"))
  V10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.V10.Rds"))
  model_d01_WD   <- uv2wd(u = U10, v = V10)
  saveRDS(model_d01_WD,paste0(WRF_folder,"/",case,"/metar.d01.WD.Rds"))
}

cat("opening WD:\\n")
files_obs <- dir(path = METAR_folder,pattern = ".Rds",full.names = T)
obs       <- data.frame(date = model_d01_WD$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("open",files_obs[i],i,"of",length(files_obs),"\\n")
  new <- readRDS(files_obs[i])
  if(nrow(new) > 1 & !all(is.na(new$WD)) ){
    name       <- new$name[1]
    new        <- data.frame(date = new$date,
                             obs  = new$WD)

    cat("station name:",name,"\\n")
    names(new) <- c("date",name)    # renaming WD for the station name
    new        <- new[!duplicated(new$date), ]
    obs        <- merge(obs, new, by = "date",all.x = T,sort = TRUE)
  }else{
    cat("no data selected in:",files_obs[i],"\\n")
  }
}
observed_wd <- obs

cat("WD for d01:\\n")

mod_stats_d01_wd <- data.frame()

for(i in names(model_d01_WD)[-1]){
  mod_stats_d01_wd <- eva(mo = model_d01_WD,
                          ob = observed_wd,
                          table = mod_stats_d01_wd,
                          wd = TRUE,
                          site  = i)
}
mod_stats_d01_wd   <- mod_stats_d01_wd[mod_stats_d01_wd$n > 1, ]  # remove stations w/no data
mod_stats_d01_wd   <- eva(model_d01_WD,observed_wd,wd = TRUE,table = mod_stats_d01_wd)
cat("...\\n")
print(tail(mod_stats_d01_wd))
cat("\\n")

write_stat(stat = mod_stats_d01_wd,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WD.d01.csv"))
  '),
      file = paste0(root,'table_metar_WD.R'),
      append = FALSE)

  if(verbose)
    cat(' r-script',paste0(root,'all_tables.R'),': setup and run script
 r-script',paste0(root,'table_metar_T2.R'),': evaluation of Temperature using METAR
 r-script',paste0(root,'table_metar_Q2.R'),': evaluation of absolute humidity using METAR
 r-script',paste0(root,'table_metar_WS.R'),': evaluation of wind speed using METAR
 r-script',paste0(root,'table_metar_WD.R'),': evaluation of wind direction using METAR\n')
}


### SCRIPT TO SATELLITE EVALUATION USING GPCP
if(template == 'SAT'){
  dir.create(path = paste0(root,'WRF/',case),
             recursive = TRUE,
             showWarnings = FALSE)
  dir.create(path = paste0(root,'GPCP'),
             recursive = TRUE,
             showWarnings = FALSE)

  cat('library(terra)  # read sat data
library(eva3dm) # read wrf and evaluate

case         = "',case,'"

file_GPCP       <- "g4.timeAvgMap.GPCPMON_3_2_sat_gauge_precip.20230401-20230430.180W_90S_180E_90N.nc"
file_WRF_RAINC  <- "WRF.d01.rain.2023-04.nc"
file_WRF_RAINNC <- "WRF.d01.rain.2023-04.nc"

m      <- vect(paste(system.file("extdata", package = "eva3dm"),"/coast.shp", sep=""))

GPCP   <- rast(paste0("GPCP/",file_GPCP))
RAINC  <- wrf_rast(paste0("WRF/",case,"/",file_WRF_RAINC),"RAINC", verbose = T)
RAINNC <- wrf_rast(paste0("WRF/",case,"/",file_WRF_RAINNC),"RAINNC",verbose = T)

RAIN   <- rain(rainc   = RAINC,
               rainnc  = RAINNC,
               verbose = TRUE)

precip = 24 * mean(RAIN, na.rm = T)

table <- sat(mo = precip,ob = GPCP,mask = m,rname = paste("GPCP",case))

print(table)

write_stat(stat = table,file = paste0("stats_GPCP.csv"))

  ',
file = paste0(root,'table_GPCP_rain.R'),
append = FALSE)

  if(verbose)
    cat(' folder ',paste0(root,'WRF/',case),': copy wrf post-proced files here!
 folder ',paste0(root,'GPCP'),': folder to copy satellite data here!
 r-script',paste0(root,'table_GPCP_rain.R'),': script for satellite evaluation (CHANGE the file names!!)\n')
}

### SCRIPT TO EVALUATION USING METAR for 3 domains + fair comparison
if(template == 'MET-3'){

  dir.create(path = paste0(root,"WRF/",case),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0('library(eva3dm)

setwd("',root,'")

WRF_folder   = "WRF"
METAR_folder = "METAR"
case         = "',case,'"

min_WS       =  0.52 # use 0.5 // 1 Knots ~ 0.514444 m/s


sink(file = paste0(WRF_folder,"/",case,"/eval.log"),split = T)

source("table_metar_T2.R")
source("table_metar_Q2.R")
source("table_metar_WS.R")
source("table_metar_WD.R")

cat("CASE:",case,"DONE!\\n")
sink()

  '),
      file = paste0(root,'all_tables.R'),
      append = FALSE)


  cat(paste0('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "',case,'"

model_d01     <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.T2.Rds"))
model_d01[-1] <- model_d01[-1] - 273.15 # to convert to Celcius
model_d02     <- readRDS(paste0(WRF_folder,"/",case,"/metar.d02.T2.Rds"))
model_d02[-1] <- model_d02[-1] - 273.15 # to convert to Celcius
model_d03     <- readRDS(paste0(WRF_folder,"/",case,"/metar.d03.T2.Rds"))
model_d03[-1] <- model_d03[-1] - 273.15 # to convert to Celcius

cat("opening TEMP:\\n")
files_obs <- dir(path = METAR_folder,pattern = ".Rds",full.names = T)
obs       <- data.frame(date = model_d01$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("open",files_obs[i],i,"of",length(files_obs),"\\n")
  new <- readRDS(files_obs[i])
  if(nrow(new) > 1 & !all(is.na(new$T2)) ){
    name       <- new$name[1]
    new        <- data.frame(date = new$date,
                             obs  = new$T2)
    cat("station name:",name,"\\n")
    names(new) <- c("date",name) # renaming T2 for the station name
    new        <- new[!duplicated(new$date), ]
    obs        <- merge(obs, new, by = "date",all.x = T,sort = TRUE)
  }else{
    cat("no data selected in:",files_obs[i],"\\n")
  }
}
observed <- obs

# observed[-1] <- observed[-1] + 273.15 # Celcius to Kelvin

cat("Temperature for d01:\\n")

mod_stats_d01 <- data.frame()

for(i in names(model_d01)[-1]){
  mod_stats_d01 <- eva(mo = model_d01,
                       ob = observed,
                       table = mod_stats_d01,
                       site  = i)
}
mod_stats_d01   <- mod_stats_d01[mod_stats_d01$n > 1, ]  # remove missing data
mod_stats_d01   <- eva(model_d01,observed,"ALL",table = mod_stats_d01)
cat("...\\n")
print(tail(mod_stats_d01))
cat("\\n")

write_stat(stat = mod_stats_d01,
           file = paste0(WRF_folder,"/",case,"/stats.metar.T2.d01.csv"))

cat("Temperature for d02:\\n")

mod_stats_d02 <- data.frame()

for(i in names(model_d02)[-1]){
  mod_stats_d02 <- eva(mo = model_d02,
                       ob = observed,
                       table = mod_stats_d02,
                       site  = i)
}
mod_stats_d02   <- mod_stats_d02[mod_stats_d02$n > 1, ]  # remove missing data
mod_stats_d02   <- eva(model_d02,observed,"ALL",table = mod_stats_d02)
cat("...\\n")
print(tail(mod_stats_d02))
cat("\\n")

write_stat(stat = mod_stats_d02,
           file = paste0(WRF_folder,"/",case,"/stats.metar.T2.d02.csv"))

cat("Temperature for d03:\\n")

mod_stats_d03 <- data.frame()

for(i in names(model_d03)[-1]){
  mod_stats_d02 <- eva(mo = model_d03,
                       ob = observed,
                       table = mod_stats_d03,
                       site  = i)
}
mod_stats_d03   <- mod_stats_d03[mod_stats_d03$n > 1, ]  # remove missing data
mod_stats_d03   <- eva(model_d03,observed,"ALL",table = mod_stats_d03)
cat("...\\n")
print(tail(mod_stats_d03))
cat("\\n")

write_stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,"/",case,"/stats.metar.T2.d03.csv"))

# fair comparison for d01 / d02 / d03
summary_stats <- rbind("d01 in d01" = eva(model_d01,observed,"ALL",fair = model_d01),
                       "d01 in d02" = eva(model_d01,observed,"ALL",fair = model_d02),
                       "d02 in d02" = eva(model_d02,observed,"ALL",fair = model_d02),
                       "d01 in d03" = eva(model_d01,observed,"ALL",fair = model_d03),
                       "d02 in d03" = eva(model_d02,observed,"ALL",fair = model_d03),
                       "d03 in d03" = eva(model_d03,observed,"ALL",fair = model_d03))

print(summary_stats)

write_stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,"/",case,"/stats.metar.T2.all.csv"))

  '),
      file = paste0(root,'table_metar_T2.R'),
      append = FALSE)

  cat(paste0('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "',case,'"

model_d01        <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.Q2.Rds"))
model_d02        <- readRDS(paste0(WRF_folder,"/",case,"/metar.d02.Q2.Rds"))
model_d03        <- readRDS(paste0(WRF_folder,"/",case,"/metar.d03.Q2.Rds"))

cat("opening Q2:\\n")
files_obs <- dir(path = METAR_folder,pattern = ".Rds",full.names = T)
obs       <- data.frame(date = model_d01$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("open",files_obs[i],i,"of",length(files_obs),"\\n")
  new <- readRDS(files_obs[i])
  if(nrow(new) > 1 & !all(is.na(new$Q2)) ){
    name       <- new$name[1]
    new        <- data.frame(date = new$date,
                             obs  = new$Q2)

    cat("station name:",name,"\\n")
    names(new) <- c("date",name)    # renaming Q2 for the station name
    new        <- new[!duplicated(new$date), ]
    obs        <- merge(obs, new, by = "date",all.x = T,sort = TRUE)
  }else{
    cat("no data selected in:",files_obs[i],"\\n")
  }
}
observed <- obs

cat("convert units to kg/g\\n")
observed[-1]  <- observed[-1]  * 1000
model_d01[-1] <- model_d01[-1] * 1000
model_d02[-1] <- model_d02[-1] * 1000
model_d03[-1] <- model_d03[-1] * 1000

cat("remove obs > 40 kg/g from obs\\n")
DATA            <- observed[-1]
DATA[DATA > 40] <- NA
observed[-1]    <- DATA
rm(DATA)

# using daily average
model_d01 <- daily(data = model_d01)
model_d02 <- daily(data = model_d02)
model_d03 <- daily(data = model_d03)
observed  <- daily(data = observed)

cat("Q2 for d01:\\n")

mod_stats_d01 <- data.frame()

for(i in names(model_d01)[-1]){
  mod_stats_d01 <- eva(mo = model_d01,
                       ob = observed,
                       table = mod_stats_d01,
                       site  = i)
}
mod_stats_d01   <- mod_stats_d01[mod_stats_d01$n > 1, ]  # remove stations w/no data
mod_stats_d01   <- eva(model_d01,observed,"ALL",table = mod_stats_d01)
cat("...\\n")
print(tail(mod_stats_d01))
cat("\\n")

write_stat(stat = mod_stats_d01,
           file = paste0(WRF_folder,"/",case,"/stats.metar.Q2.d01.csv"))

cat("Q2 for d02:\\n")

mod_stats_d02 <- data.frame()

for(i in names(model_d02)[-1]){
  mod_stats_d02 <- eva(mo = model_d02,
                       ob = observed,
                       table = mod_stats_d02,
                       site  = i)
}
mod_stats_d02   <- mod_stats_d02[mod_stats_d02$n > 1, ]  # remove missing data
mod_stats_d02   <- eva(model_d02,observed,"ALL",table = mod_stats_d02)
cat("...\\n")
print(tail(mod_stats_d02))
cat("\\n")

write_stat(stat = mod_stats_d02,
           file = paste0(WRF_folder,"/",case,"/stats.metar.Q2.d02.csv"))

cat("Q2 for d03:\\n")

mod_stats_d03 <- data.frame()

for(i in names(model_d03)[-1]){
  mod_stats_d02 <- eva(mo = model_d03,
                       ob = observed,
                       table = mod_stats_d03,
                       site  = i)
}
mod_stats_d03   <- mod_stats_d03[mod_stats_d03$n > 1, ]  # remove missing data
mod_stats_d03   <- eva(model_d03,observed,"ALL",table = mod_stats_d03)
cat("...\\n")
print(tail(mod_stats_d03))
cat("\\n")

write_stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,"/",case,"/stats.metar.Q2.d03.csv"))

# fair comparison for d01 / d02 / d03
summary_stats <- rbind("d01 in d01" = eva(model_d01,observed,fair = model_d01),
                       "d01 in d02" = eva(model_d01,observed,fair = model_d02),
                       "d02 in d02" = eva(model_d02,observed,fair = model_d02),
                       "d01 in d03" = eva(model_d01,observed,fair = model_d03),
                       "d02 in d03" = eva(model_d02,observed,fair = model_d03),
                       "d03 in d03" = eva(model_d03,observed,fair = model_d03))

print(summary_stats)

write_stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,"/",case,"/stats.metar.Q2.all.csv"))
  '),
      file = paste0(root,'table_metar_Q2.R'),
      append = FALSE)

  cat(paste0('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "case"

if(file.exists(paste0(WRF_folder,"/",case,"/metar.d01.WS.Rds"))){
  cat("opening calculated wind speed for d01...\\n")
  model_d01_WS <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.WS.Rds"))
}else{
  U10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.U10.Rds"))
  V10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.V10.Rds"))
  model_d01_WS   <- uv2ws(u = U10, v = V10)
  saveRDS(model_d01_WS,paste0(WRF_folder,"/",case,"/metar.d01.WS.Rds"))
}

if(file.exists(paste0(WRF_folder,"/",case,"/metar.d02.WS.Rds"))){
  cat("opening calculated wind speed for d02...\\n")
  model_d02_WS <- readRDS(paste0(WRF_folder,"/",case,"/metar.d02.WS.Rds"))
}else{
  U10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d02.U10.Rds"))
  V10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d02.V10.Rds"))
  model_d02_WS   <- uv2ws(u = U10, v = V10)
  saveRDS(model_d02_WS,paste0(WRF_folder,"/",case,"/metar.d02.WS.Rds"))
}

if(file.exists(paste0(WRF_folder,"/",case,"/metar.d03.WS.Rds"))){
  cat("opening calculated wind speed for d03...\\n")
  model_d03_WS <- readRDS(paste0(WRF_folder,"/",case,"/metar.d03.WS.Rds"))
}else{
  U10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d03.U10.Rds"))
  V10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d03.V10.Rds"))
  model_d03_WS   <- uv2ws(u = U10, v = V10)
  saveRDS(model_d03_WS,paste0(WRF_folder,"/",case,"/metar.d03.WS.Rds"))
}

cat("opening WS:\\n")
files_obs <- dir(path = METAR_folder,pattern = ".Rds",full.names = T)
obs       <- data.frame(date = model_d01_WS$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("open",files_obs[i],i,"of",length(files_obs),"\\n")
  new <- readRDS(files_obs[i])
  if(nrow(new) > 1 & !all(is.na(new$WS)) ){
    name       <- new$name[1]
    new        <- data.frame(date = new$date,
                             obs  = new$WS)

    cat("station name:",name,"\\n")
    names(new) <- c("date",name)    # renaming WS for the station name
    new        <- new[!duplicated(new$date), ]
    obs        <- merge(obs, new, by = "date",all.x = T,sort = TRUE)
  }else{
    cat("no data selected in:",files_obs[i],"\\n")
  }
}
observed_ws <- obs

if(!is.na(min_WS)){
  cat("removing WS less than",min_WS,"\n")
  DATA                 <- observed[-1]
  DATA[DATA < min_WS ] <- NA
  observed[-1]         <- DATA
  rm(DATA)
}else{
  cat("removing WS less than zero\n")
  DATA              <- observed[-1]
  DATA[DATA < 0.0 ] <- NA
  observed[-1]      <- DATA
  rm(DATA)
}

cat("WS for d01:\\n")

mod_stats_d01_ws <- data.frame()

for(i in names(model_d01_WS)[-1]){
  mod_stats_d01_ws <- eva(mo = model_d01_WS,
                          ob = observed_ws,
                          table = mod_stats_d01_ws,
                          site  = i)
}
mod_stats_d01_ws   <- mod_stats_d01_ws[mod_stats_d01_ws$n > 1, ]  # remove stations w/no data
mod_stats_d01_ws   <- eva(model_d01_WS,observed_ws,"ALL",table = mod_stats_d01_ws)
cat("...\\n")
print(tail(mod_stats_d01_ws))
cat("\\n")

write_stat(stat = mod_stats_d01_ws,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WS.d01.csv"))

cat("WS for d02:\\n")

mod_stats_d02 <- data.frame()

for(i in names(model_d02)[-1]){
  mod_stats_d02 <- eva(mo = model_d02,
                       ob = observed,
                       table = mod_stats_d02,
                       site  = i)
}
mod_stats_d02   <- mod_stats_d02[mod_stats_d02$n > 1, ]  # remove missing data
mod_stats_d02   <- eva(model_d02,observed,"ALL",table = mod_stats_d02)
cat("...\\n")
print(tail(mod_stats_d02))
cat("\\n")

write_stat(stat = mod_stats_d02,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WS.d02.csv"))

cat(" for d03:\\n")

mod_stats_d03 <- data.frame()

for(i in names(model_d03)[-1]){
  mod_stats_d02 <- eva(mo = model_d03,
                       ob = observed,
                       table = mod_stats_d03,
                       site  = i)
}
mod_stats_d03   <- mod_stats_d03[mod_stats_d03$n > 1, ]  # remove missing data
mod_stats_d03   <- eva(model_d03,observed,"ALL",table = mod_stats_d03)
cat("...\\n")
print(tail(mod_stats_d03))
cat("\\n")

write_stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WS.d03.csv"))

# fair comparison for d01 / d02 / d03
summary_stats <- rbind("d01 in d01" = eva(model_d01,observed,fair = model_d01),
                       "d01 in d02" = eva(model_d01,observed,fair = model_d02),
                       "d02 in d02" = eva(model_d02,observed,fair = model_d02),
                       "d01 in d03" = eva(model_d01,observed,fair = model_d03),
                       "d02 in d03" = eva(model_d02,observed,fair = model_d03),
                       "d03 in d03" = eva(model_d03,observed,fair = model_d03))

print(summary_stats)

write_stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WS.all.csv"))
  '),
      file = paste0(root,'table_metar_WS.R'),
      append = FALSE)

  cat(paste0('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "case"

if(file.exists(paste0(WRF_folder,"/",case,"/metar.d01.WD.Rds"))){
  cat("opening calculated wind speed for d01...\\n")
  model_d01_WD <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.WD.Rds"))
}else{
  U10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.U10.Rds"))
  V10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d01.V10.Rds"))
  model_d01_WD   <- uv2wd(u = U10, v = V10)
  saveRDS(model_d01_WD,paste0(WRF_folder,"/",case,"/metar.d01.WD.Rds"))
}

if(file.exists(paste0(WRF_folder,"/",case,"/metar.d02.WD.Rds"))){
  cat("opening calculated wind speed for d02...\\n")
  model_d02_WD <- readRDS(paste0(WRF_folder,"/",case,"/metar.d02.WD.Rds"))
}else{
  U10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d02.U10.Rds"))
  V10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d02.V10.Rds"))
  model_d02_WD   <- uv2wd(u = U10, v = V10)
  saveRDS(model_d02_WD,paste0(WRF_folder,"/",case,"/metar.d02.WD.Rds"))
}

if(file.exists(paste0(WRF_folder,"/",case,"/metar.d03.WD.Rds"))){
  cat("opening calculated wind speed for d03...\\n")
  model_d03_WD <- readRDS(paste0(WRF_folder,"/",case,"/metar.d03.WD.Rds"))
}else{
  U10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d03.U10.Rds"))
  V10  <- readRDS(paste0(WRF_folder,"/",case,"/metar.d03.V10.Rds"))
  model_d03_WD   <- uv2wd(u = U10, v = V10)
  saveRDS(model_d03_WD,paste0(WRF_folder,"/",case,"/metar.d03.WD.Rds"))
}

cat("opening WD:\\n")
files_obs <- dir(path = METAR_folder,pattern = ".Rds",full.names = T)
obs       <- data.frame(date = model_d01_WD$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("open",files_obs[i],i,"of",length(files_obs),"\\n")
  new <- readRDS(files_obs[i])
  if(nrow(new) > 1 & !all(is.na(new$WD)) ){
    name       <- new$name[1]
    new        <- data.frame(date = new$date,
                             obs  = new$WD)

    cat("station name:",name,"\\n")
    names(new) <- c("date",name)    # renaming WD for the station name
    new        <- new[!duplicated(new$date), ]
    obs        <- merge(obs, new, by = "date",all.x = T,sort = TRUE)
  }else{
    cat("no data selected in:",files_obs[i],"\\n")
  }
}
observed_wd <- obs

cat("WD for d01:\\n")

mod_stats_d01_wd <- data.frame()

for(i in names(model_d01_WD)[-1]){
  mod_stats_d01_wd <- eva(mo = model_d01_WD,
                          ob = observed_wd,
                          table = mod_stats_d01_wd,
                          wd = TRUE,
                          site  = i)
}
mod_stats_d01_wd   <- mod_stats_d01_wd[mod_stats_d01_wd$n > 1, ]  # remove stations w/no data
mod_stats_d01_wd   <- eva(model_d01_WD,observed_wd,wd = TRUE,table = mod_stats_d01_wd)
cat("...\\n")
print(tail(mod_stats_d01_wd))
cat("\\n")

write_stat(stat = mod_stats_d01_wd,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WD.d01.csv"))

cat("WD for d02:\\n")

mod_stats_d02 <- data.frame()

for(i in names(model_d02)[-1]){
  mod_stats_d02 <- eva(mo = model_d02,
                       ob = observed,
                       table = mod_stats_d02,
                       wd = TRUE,
                       site  = i)
}
mod_stats_d02   <- mod_stats_d02[mod_stats_d02$n > 1, ]  # remove missing data
mod_stats_d02   <- eva(model_d02,observed,wd = TRUE,table = mod_stats_d02)
cat("...\\n")
print(tail(mod_stats_d02))
cat("\\n")

write_stat(stat = mod_stats_d02,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WD.d02.csv"))

cat("WD for d03:\\n")

mod_stats_d03 <- data.frame()

for(i in names(model_d03)[-1]){
  mod_stats_d02 <- eva(mo = model_d03,
                       ob = observed,
                       table = mod_stats_d03,
                       wd = TRUE,
                       site  = i)
}
mod_stats_d03   <- mod_stats_d03[mod_stats_d03$n > 1, ]  # remove missing data
mod_stats_d03   <- eva(model_d03,observed,wd = TRUE,table = mod_stats_d03)
cat("...\\n")
print(tail(mod_stats_d03))
cat("\\n")

write_stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WD.d03.csv"))

# fair comparison for d01 / d02 / d03
summary_stats <- rbind("d01 in d01" = eva(model_d01,observed,fair = model_d01,wd = TRUE),
                       "d01 in d02" = eva(model_d01,observed,fair = model_d02,wd = TRUE),
                       "d02 in d02" = eva(model_d02,observed,fair = model_d02,wd = TRUE),
                       "d01 in d03" = eva(model_d01,observed,fair = model_d03,wd = TRUE),
                       "d02 in d03" = eva(model_d02,observed,fair = model_d03,wd = TRUE),
                       "d03 in d03" = eva(model_d03,observed,fair = model_d03,wd = TRUE))

print(summary_stats)

write_stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,"/",case,"/stats.metar.WD.all.csv"))
  '),
      file = paste0(root,'table_metar_WD.R'),
      append = FALSE)

  if(verbose)
    cat(' r-script',paste0(root,'all_tables.R'),': setup and run script
 r-script',paste0(root,'table_metar_T2.R'),': evaluation of Temperature using METAR
 r-script',paste0(root,'table_metar_Q2.R'),': evaluation of absolute humidity using METAR
 r-script',paste0(root,'table_metar_WS.R'),': evaluation of wind speed using METAR
 r-script',paste0(root,'table_metar_WD.R'),': evaluation of wind direction using METAR\n')
}

### SETUP for extract CAMx for 3 domains
if(template == 'CAMx'){
  dir.create(path = paste0(root,'CAMx/',case),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0(HEADER,'

dir=\'',case,'\'

cd ',root,'

conda activate ',env,'

echo \'folder:\' $dir

date

Rscript extract_camx.R        $dir O3        &
Rscript extract_camx.R        $dir NO        &
Rscript extract_camx.R        $dir NO2       &
Rscript extract_camx.R        $dir FORM      &
Rscript extract_camx.R        $dir CO        &
Rscript extract_camx.R        $dir SO2       &

wait

# extract PM time-series 17
# PM2.5 = PSO4 + PNO3 + PNH4 + NA + PCL + PEC + POA + SOA1-4 + SOPA + SOPB + FCRS + FPRM
Rscript extract_camx.R        $dir PSO4      &
Rscript extract_camx.R        $dir PNO3      &
Rscript extract_camx.R        $dir PNH4      &
Rscript extract_camx.R        $dir NA        &
Rscript extract_camx.R        $dir PCL       &
Rscript extract_camx.R        $dir PEC       &
Rscript extract_camx.R        $dir POA       &
Rscript extract_camx.R        $dir SOA1      &
Rscript extract_camx.R        $dir SOA2      &
Rscript extract_camx.R        $dir SOA3      &
Rscript extract_camx.R        $dir SOA4      &
Rscript extract_camx.R        $dir SOPA      &
Rscript extract_camx.R        $dir SOPB      &
Rscript extract_camx.R        $dir FCRS      &
Rscript extract_camx.R        $dir FPRM      &
# PM10 = PM2.5 + CCRS + CPRM
Rscript extract_camx.R        $dir CCRS      &
Rscript extract_camx.R        $dir CPRM      &
#Rscript extract_camx.R       $dir PH2O      &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
mv exp.d0* WRF/$dir
'),
      file = paste0(root,'post-R_CAMx.sh'),
      append = FALSE)

  cat('args <- commandArgs(trailingOnly = TRUE)

library(hackWRF)

dir  <- args[1]

var  <- args[2]
if(length(args) > 2){
  ndim <- args[3]
}else{
  ndim <- "4d"
}

if(ndim == "&")
  ndim <- "4d"

stations <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AERONET.Rds"))

files    <- dir(path = dir, pattern = "grd01.nc",full.names = T)
extract_serie(filelist = files,
              new      = T,
              point    = stations,
              variable = var,
              field    = ndim,
              latitude = "latitude",
              longitude = "longitude",
              use_TFLAG = T,
              prefix   = "aeronet.d01")

files    <- dir(path = dir, pattern = "grd02.nc",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = stations,
              variable = var,
              field    = ndim,
              latitude = "latitude",
              longitude = "longitude",
              use_TFLAG = T,
              prefix   = "aeronet.d02")

files    <- dir(path = dir, pattern = "grd03.nc",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = stations,
              variable = var,
              field    = ndim,
              latitude = "latitude",
              longitude = "longitude",
              use_TFLAG = T,
              prefix   = "aeronet.d03")

  ',
file = paste0(root,'extract_camx.R'),
append = FALSE)

  if(verbose)
    cat(' folder ',paste0(root,'CAMx/',case),': link CAMx output files here!
 bash ',   paste0(root,'post-R_CAMx.sh'),': post processing job script
 r-script',paste0(root,'extract_camx.R'),': source code to extract time-series from AERONET site locations using eva3dm::extract_serie()\n')
}


### SETUP for extract CAMx for 3 domains
if(template == 'CAMx'){
  dir.create(path = paste0(root,'CAMx/',case),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0(HEADER,'

dir=\'',case,'\'

cd ',root,'

conda activate ',env,'

echo \'folder:\' $dir

date

Rscript extract_camx.R        $dir O3        &
Rscript extract_camx.R        $dir NO        &
Rscript extract_camx.R        $dir NO2       &
Rscript extract_camx.R        $dir FORM      &
Rscript extract_camx.R        $dir CO        &
Rscript extract_camx.R        $dir SO2       &

wait

# extract PM time-series 17
# PM2.5 = PSO4 + PNO3 + PNH4 + NA + PCL + PEC + POA + SOA1-4 + SOPA + SOPB + FCRS + FPRM
Rscript extract_camx.R        $dir PSO4      &
Rscript extract_camx.R        $dir PNO3      &
Rscript extract_camx.R        $dir PNH4      &
Rscript extract_camx.R        $dir NA        &
Rscript extract_camx.R        $dir PCL       &
Rscript extract_camx.R        $dir PEC       &
Rscript extract_camx.R        $dir POA       &
Rscript extract_camx.R        $dir SOA1      &
Rscript extract_camx.R        $dir SOA2      &
Rscript extract_camx.R        $dir SOA3      &
Rscript extract_camx.R        $dir SOA4      &
Rscript extract_camx.R        $dir SOPA      &
Rscript extract_camx.R        $dir SOPB      &
Rscript extract_camx.R        $dir FCRS      &
Rscript extract_camx.R        $dir FPRM      &
# PM10 = PM2.5 + CCRS + CPRM
Rscript extract_camx.R        $dir CCRS      &
Rscript extract_camx.R        $dir CPRM      &
#Rscript extract_camx.R       $dir PH2O      &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
mv exp.d0* WRF/$dir
'),
      file = paste0(root,'post-R_CAMx.sh'),
      append = FALSE)

  cat('args <- commandArgs(trailingOnly = TRUE)

library(hackWRF)

dir  <- args[1]

var  <- args[2]
if(length(args) > 2){
  ndim <- args[3]
}else{
  ndim <- "4d"
}

if(ndim == "&")
  ndim <- "4d"

stations <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AERONET.Rds"))

files    <- dir(path = dir, pattern = "grd01.nc",full.names = T)
extract_serie(filelist = files,
              new      = T,
              point    = stations,
              variable = var,
              field    = ndim,
              latitude = "latitude",
              longitude = "longitude",
              use_TFLAG = T,
              prefix   = "aeronet.d01")

files    <- dir(path = dir, pattern = "grd02.nc",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = stations,
              variable = var,
              field    = ndim,
              latitude = "latitude",
              longitude = "longitude",
              use_TFLAG = T,
              prefix   = "aeronet.d02")

files    <- dir(path = dir, pattern = "grd03.nc",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = stations,
              variable = var,
              field    = ndim,
              latitude = "latitude",
              longitude = "longitude",
              use_TFLAG = T,
              prefix   = "aeronet.d03")

  ',
file = paste0(root,'extract_camx.R'),
append = FALSE)

  if(verbose)
    cat(' folder ',paste0(root,'CAMx/',case),': link CAMx output files here!
 bash ',   paste0(root,'post-R_CAMx.sh'),': post processing job script
 r-script',paste0(root,'extract_camx.R'),': source code to extract time-series from AERONET site locations using eva3dm::extract_serie()\n')
}

### SCRIPT TO EVALUATION of AQ
if(template == 'AQ'){

  dir.create(path = paste0(root,"WRF/",case),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0('library(eva3dm)

setwd("',root,'")

case     = "',case,'"

cat("Air Quality Stations\\n")
source("table_aq_o3.R")
source("table_aq_max_o3.R")
source("table_aq_pm25.R")
#source("table_aq_pm10.R")
#source("table_aq_no.R")
#source("table_aq_no2.R")
#source("table_aq_co.R")
#source("table_aq_so2.R")
#cat("Meteorology from AQS\\n")
#source("table_aq_T2.R")
#source("table_aq_Q2.R")
#source("table_aq_WS.R")
#source("table_aq_WD.R")
#source("table_aq_rain.R")

#cat("INMET\\n")
#source("table_inmet_T2.R")
#source("table_inmet_Q2.R")
#source("table_inmet_WS.R")
#source("table_inmet_WD.R")
#source("table_inmet_rain.R")

#cat("METAR\\n")
#source("table_metar_T2.R")
#source("table_metar_Q2.R")
#source("table_metar_WS.R")
#source("table_metar_WD.R")

cat("DONE!\\n")
  '),
      file = paste0(root,'all_tables.R'),
      append = FALSE)

  cat('# library(eva3dm)

variable      = "O3"
o3_NME_cutoff = 78.4 # 78.4 (40 ppb) cutoff for O3 NME // NA

model       <- readRDS(paste0("WRF/",case,"/serie.o3.Rds"))
TEMP        <- readRDS(paste0("WRF/",case,"/serie.T2.Rds"))
model[,-1]  <- model[,-1] * 10^3*(48)/(0.0805 * TEMP[,-1])   # 48 = O3 molar mass

files_obs <- dir(path = paste0("OBS/"),pattern = paste0("_",variable,".Rds"),full.names = T)
obs       <- data.frame(date = model$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("opening",files_obs[i],"\\n")
  new       <- readRDS(files_obs[i])[,c(2,4)] # column 2 is date in POSIXct and column 4 is O3 in ug/m3
  obs       <- suppressWarnings( merge(obs, new, by = "date",all.x = T,sort = TRUE) )
}
# using the file names for station names
names(obs) <- c("date",substr(files_obs,nchar(paste0("OBS/"))+8,nchar(files_obs)-7))
observed   <- obs

# calculate moving 8-hour average
model      <- ma8h(model)
observed   <- ma8h(observed)

cat("8h-O3 evaluation:\\n")
table <- data.frame()
for(i in names(model)[-1]){
  table <- eva(mo = model,
               ob = observed,
               table = table,
               site = i)
}

table <- eva(ob = observed,
             mo = model,
             table = table,
             cutoff_NME = o3_NME_cutoff)

print(table)
cat("\\n")

write_stat(stat = table,
           file = paste0("WRF/",case,"/stats.aq.8h.",variable,".csv"))

  ',
file = paste0(root,'table_aq_o3.R'),
append = FALSE)

  cat('# library(eva3dm)

variable      = "O3"
o3_NME_cutoff = 78.4 # 78.4 (40 ppb) cutoff for O3 NME // NA

model       <- readRDS(paste0("WRF/",case,"/serie.o3.Rds"))
TEMP        <- readRDS(paste0("WRF/",case,"/serie.T2.Rds"))
model[,-1]  <- model[,-1] * 10^3*(48)/(0.0805 * TEMP[,-1])   # 48 = O3 molar mass

files_obs <- dir(path = paste0("OBS/"),pattern = paste0("_",variable,".Rds"),full.names = T)
obs       <- data.frame(date = model$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("opening",files_obs[i],"\\n")
  new       <- readRDS(files_obs[i])[,c(2,4)] # column 2 is date in POSIXct and column 4 is O3 in ug/m3
  obs       <- suppressWarnings( merge(obs, new, by = "date",all.x = T,sort = TRUE) )
}
# using the file names for station names
names(obs) <- c("date",substr(files_obs,nchar(paste0("OBS/"))+8,nchar(files_obs)-7))
observed   <- obs

# calculate daily maximum 8-hour average
model    <- mda8(model)
observed <- mda8(observed)

cat("MAD8 O3 evaluation:\\n")
table <- data.frame()
for(i in names(model)[-1]){
  table <- eva(mo = model,
               ob = observed,
               table = table,
               site = i)
}

table <- eva(ob = observed,
             mo = model,
             table = table,
             cutoff_NME = o3_NME_cutoff)

print(table)
cat("\\n")

write_stat(stat = table,
           file = paste0("WRF/",case,"/stats.aq.max.8h.",variable,".csv"))

  ',
file = paste0(root,'table_aq_max_o3.R'),
append = FALSE)

  cat('# library(eva3dm)

variable = "PM2.5" # ug/m3

model       <- readRDS(paste0("WRF/",case,"/serie.PM2_5_DRY.Rds"))

files_obs <- dir(path = paste0("OBS/"),pattern = paste0("_",variable,".Rds"),full.names = T)
obs       <- data.frame(date = model$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat("opening",files_obs[i],"\\n")
  new       <- readRDS(files_obs[i])[,c(2,4)] # column 2 is date in POSIXct and column 4 is PM2.5 in ug/m3
  obs       <- suppressWarnings( merge(obs, new, by = "date",all.x = T,sort = TRUE) )
}
# using the file names for station names
names(obs) <- c("date",substr(files_obs,nchar(paste0("OBS/"))+8,nchar(files_obs)-10))
observed   <- obs

# calculate daily average
model    <- daily(model)
observed <- daily(observed)

cat("Daily PM2.5 evaluation:\\n")
table <- data.frame()
for(i in names(model)[-1]){
  table <- eva(mo = model,
               ob = observed,
               table = table,
               site = i,
               cutoff = c(1,150))
}

table <- eva(ob = observed,
             mo = model,
             table = table,
             cutoff = c(1,150))

print(table)
cat("\\n")

write_stat(stat = table,
           file = paste0("WRF/",case,"/stats.aq.daily.",variable,".csv"))

  ',
file = paste0(root,'table_aq_pm25.R'),
append = FALSE)

  if(verbose)
    cat(' folder ',paste0(root,'WRF/',case),': link WRF or CAMx post-rocessed output here!
 r-script',paste0(root,'all_tables.R'),': setup and run script
 r-script',paste0(root,'table_aq_o3.R'),': evaluation of hourly 8h O3
 r-script',paste0(root,'table_aq_max_o3.R'),': evaluation of daily max 8h O3
 r-script',paste0(root,'table_aq_pm2.5.R'),': evaluation of daily pm2.5
 NOTE 1: Other scripts in all_tables.R not provided, use previous as template
 NOTE 2: other templates provide templates for metar\n')
}

### SETUP for post process WRF for satellite evaluation
if(template == 'PSA'){
  dir.create(path = paste0(root,'WRF/',case),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0(HEADER,'

cd ',root,'

year=2023         # year to be processed
month=04          # month to be processed
domain=d01        # used only to label output
output="WRF/',case,'" # folder to save the post processing

# map.d0[1-4].nc files
ncks -d Time,1 -v XLAT,XLONG ${output}/wrfout_d01_${year}-01-01_00:00:00 ${output}/map.${domain}.nc
# meteorological parameters to calculate model high, layer thickness, etc
ncra -v Times,XLAT,XLONG,ALT,PB,P,T,PHB,PH,HGT ${year}/wrfout_d01_${year}-01-01* ${output}/WRF.${domain}.meteorological.nc

#for month in 01 04; do
   echo "processing WRF-Chem output for" ${year}-${month}

   # PRECIPITATION
   ncrcat -v Times,XLAT,XLONG,RAINC,RAINNC ${output}/wrfout_d01_${year}-${month}-* ${output}/WRF.${domain}.rain.${year}-${month}.nc &

   # for CERES
   for species in GLW GSW LWCF SWCF SWDOWN OLR; do
      ncra -v Times,XLAT,XLONG,${species} ${output}/wrfout_d01_${year}-${month}-* ${output}/WRF.${domain}.${species}.${year}-${month}.nc &
   done

   # for MODIS vairables
   for species in QNDROP CCN5 CLDFRA TAUCLDI TAUCLDC QCLOUD TAUAER1 TAUAER2 TAUAER3 TAUAER4; do
      ncra -v Times,XLAT,XLONG,${species} ${output}/wrfout_d01_${year}-${month}-* ${output}/WRF.${domain}.${species}.${year}-${month}.nc &
   done

   # species evaluation: SCIAMACHY MOPITT OMI AIRS etc
   for species in co no2 form so2 o3; do
      ncra -v Times,XLAT,XLONG,${species} ${output}/wrfout_d01_${year}-${month}-* ${output}/WRF.${domain}.${species}.${year}-${month}.nc &
   done

   wait
#done

echo "done!"
'),
      file = paste0(root,'post-sate.sh'),
      append = FALSE)

  if(verbose)
    cat(' bash ', paste0(root,'post-sate.sh'),': post processing for satellite evaluation using CDO\n')
}


### SETUP to download data from all stations from Qualar network/CETESB (Sao Paulo) and Monitor AR network (Rio de Janeiro) in Brazil
if(template == 'AQS_BR'){
  dir.create(path = paste0(root,'AQS_BR/'),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0('library(qualR)
## more info : https://docs.ropensci.org/qualR

# SET the OUTPUT FOLDER
folder <- "',paste0(root,'AQS_BR/'),'"
dir.create(path = folder,showWarnings = FALSE,recursive = TRUE)

## SAVE THE SITE-LIST
cetesb_sites <- cetesb_aqs
rio_sites    <- monitor_ar_aqs

all_sites <- data.frame(lat = c(cetesb_sites$lat, rio_sites$lat),
                        lon = c(cetesb_sites$lon, rio_sites$lon),
                        row.names = c(cetesb_sites$name, rio_sites$name),
                        stringsAsFactors = FALSE)

saveRDS(all_sites,paste0(folder,"/site-list.Rds")) # in Sao Paulo and Rio de Janeiro states

## to save QUALAR CREDENTIALS
## https://qualar.cetesb.sp.gov.br/qualar
# library(usethis)
# edit_r_environ()
# include user and password credentials on the file
# QUALAR_USER="underschuch@gmail.com"
# QUALAR_PASS="666"

START <- "25/06/2018"
END   <- "02/08/2018"

## to download data for the state of Sao Paulo from CETESB"s Qualar network
for(PAR in c("O3","NO","NO2","NOx","SO2","CO","MP2.5","MP10","TEMP","UR","VV","DV")){

  # get parameters (pol and met) for each AQS
  all_o3 <- lapply(cetesb_aqs$code, cetesb_retrieve_param,
                   username   = Sys.getenv("QUALAR_USER"),
                   password   = Sys.getenv("QUALAR_PASS"),
                   parameters = PAR,
                   start_date = START,
                   end_date   = END)

  # convert the list in data.frame
  all_o3_csv <- do.call(rbind, all_o3) # columns 1:date 2:variable 3:station_name

  # processing for eva3dm::eva()
  output        <- data.frame(time = all_o3[[1]][,1]) # data is common for all stations
  for(i in 1:length(all_o3)){
    output      <- cbind(output,all_o3[[i]][,2])
  }
  names(output) <- c("date",unique(all_o3_csv[,3]))
  saveRDS(output, paste0(folder,"/cetesb_",PAR,".Rds"))
}

cat("NOTE: DV (or wind direction) = 777 ou 666 are calm winds, use NA!\\n")

## to download data for the state of Rio de Janeiro Monitor AR network
for(PAR in c("O3","NO","NO2","NOx","SO2","CO","PM2_5","PM10","Temp","UR","Vel_Vento","Dir_Vento","Chuva")){

  # get parameters (pol and met) for each AQS
  rio_all <- lapply(rio_sites$code, monitor_ar_retrieve_param,
                    parameters = PAR,
                    start_date = START,
                    end_date   = END)

  # convert the list in data.frame
  rio_csv <- do.call(rbind, rio_all) # columns 1:date 2:variable 3:station_name

  # processing for eva3dm::eva()
  output        <- data.frame(time = all_o3[[1]][,1]) # data is common for all stations
  for(i in 1:length(all_o3)){
    output      <- cbind(output,all_o3[[i]][,2])
  }
  names(output) <- c("date",unique(all_o3_csv[,3]))
  saveRDS(output, paste0(folder,"/monitor_ar_",PAR,".Rds"))
}

#  to keep the same filename from CETESB data
PAR = c("Vel_Vento","Dir_Vento","PM2_5","Temp")
NEW = c("VV","DV","PM2.5","TEMP")
for(i in 1:length(PAR)){
  file.rename(from = paste0(folder,"/monitor_ar_",PAR[i],".Rds"),
              to =   paste0(folder,"/monitor_ar_",NEW[i],".Rds"))
}

cat("NOTES:
------
CETESB QUALAR system describes midnight as 24:00, and the first hour of each day starts at 1:00. qualR transform it to get the time in 00-23 hour notation, for that reason you\'ll get NA at 00:00 of your first downloaded day. So, consider download one day before your study period.
To pad-out with NA when there is a missing date, qualR tricks the date information, an assume it\'s on UTC (when in reality it\'s on America/Sao_Paulo time). This avoids problems with merging data frames and also with Daylight saving time (DST) issues. Beware of this,when dealing with study periods that include DST. It always a good idea, to double check by retrieving the suspicious date from CETESB QUALAR system.
Take into account that in CETESB data, the hourly averaged is the mean until the hour. That is, a concentration value for 22:00 is the mean from 21:01 to 22:00.
Consider the previous three points if you need to change from local time to UTC.
Currently, MonitorAr only has data until March, 2021.
")
'),
      file = paste0(root,'download_AQS_BR.R'),
      append = FALSE)

  if(verbose)
    cat(' R-Script ',paste0(root,'download_AQS_BR.R'),': download data from Qualar network/CETESB (Sao Paulo) and Monitor AR network (Rio de Janeiro)\n')
}

### script to process INMET stations
if(template == 'INMET'){
  dir.create(path = paste0(root,'INMET/automatica/'),
             recursive = TRUE,
             showWarnings = FALSE)
  dir.create(path = paste0(root,'INMET/convencional/'),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0('library(eva3dm)

folder <- "',paste0(root,'INMET/'),'"
setwd(folder)

files <- dir(path = "INMET/automatica/",pattern = ".csv",full.names = TRUE)

for(i in 1:length(files)){
  cat("* file",i,"of",length(files),files[i],"\\n")

  header       <- read.csv2(files[i],nrows = 8,header = F,stringsAsFactors = F)
  station_code <- as.character(header$V1[2])
  station_code <- substr(x     = station_code,
                         start = nchar(station_code) - 3,
                         stop  = nchar(station_code))
  cat("station code:",station_code,"\\n")

  DATA <- read.csv2(file = files[i],skip = 9,dec = ",",stringsAsFactors = F,na.strings = "null")
  if(nrow(DATA) == 0)
    next
  DATA <- DATA[-23]
  names(DATA) <- c("date", "hour",
                   "rain",
                   "pres","press_mar","press_max","press_min",
                   "rad",
                   "cpu_temp",
                   "temp","Dew_point","temp_max","temp_min","DP_max","DP_min",
                   "Bateria",
                   "RU_max","RU_min","RU",
                   "WD","gust","WS")

  datas <- as.POSIXct(x = paste(DATA$date,DATA$hour),tz = "GMT")

  for(VAR in c("rain","temp","rad","RU","WS","WD")){
    OBS   <- data.frame(site  = rep(station_code,length(datas)),
                        date  = datas,
                        par   = rep(VAR,length(datas)),
                        value = as.numeric(DATA[,VAR]),
                        stringsAsFactors = F)

    filename <- paste0("INMET_",station_code,"_",VAR,".Rds")
    cat("saving",VAR,":",filename,"...\\n")
    saveRDS(OBS,filename)
  }
}
'),
      file = paste0(root,'process_INMET_auto.R'),
      append = FALSE)

  cat(paste0('library(eva3dm)

folder <- "',paste0(root,'INMET/'),'"
setwd(folder)

files <- dir(path = "INMET/convencional/",pattern = ".csv",full.names = TRUE)

for(i in 1:length(files)){
  cat("* file",i,"of",length(files),files[i],"\\n")

  header       <- read.csv2(files[i],nrows = 8,header = F,stringsAsFactors = F)
  station_code <- as.character(header$V1[2])
  station_code <- substr(x     = station_code,
                         start = nchar(station_code) - 4,
                         stop  = nchar(station_code))
  cat("station code:",station_code,"\\n")

  DATA <- read.csv2(file = files[i],skip = 9,dec = ",",stringsAsFactors = F,na.strings = "null")
  if(nrow(DATA) == 0)
    next
  DATA <- DATA[-17]
  names(DATA) <- c("dia", "hora",
                   "nuvem_alta","nuvem_baixa","nuvem_media",
                   "nebulosidade",
                   "rain",
                   "pres","press_mar",
                   "temp","dew_point","dew_point",
                   "RU","WD","WS","visibiliadade")

  datas <- as.POSIXct(x = paste0(DATA$dia," ",as.numeric(DATA$hora)/100,":00"),tz = "GMT")

  for(VAR in c("rain","temp","RU","WS","WD")){
    OBS   <- data.frame(site  = rep(station_code,length(datas)),
                        date  = datas,
                        par   = rep(VAR,length(datas)),
                        value = as.numeric(DATA[,VAR]),
                        stringsAsFactors = F)

    filename <- paste0("INMET_",station_code,"_",VAR,".Rds")
    cat("saving",VAR,":",filename,"...\\n")
    saveRDS(OBS,filename)
  }
}
'),
      file = paste0(root,'process_INMET_conv.R'),
      append = FALSE)

  if(verbose){
    cat(' R-Script ', paste0(root,'process_INMET_auto.R'),': script to process INMET automatic stations\n')
    cat(' R-Script ', paste0(root,'process_INMET_conv.R'),': script to process INMET conventional stations\n')
    cat(' folder ',   paste0(root,'INMET/automatica/'),   ': place INMET data from automatic stations \n')
    cat(' folder ',   paste0(root,'INMET/convencional/'), ': place INMET data from conventional stations \n')
    cat(' folder ',   paste0(root,'INMET/'),              ': output folder\n')
  }
}

### script to merge INMET stations in one file, and merge METAR stations in one file
if(template == 'merge'){
  dir.create(path = paste0(root,'INMET/'),
             recursive = TRUE,
             showWarnings = FALSE)
  dir.create(path = paste0(root,'METAR/'),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0('library(eva3dm)

cat("mergin all INMET data\\n")

output_prefix <- "INMET_all_stations"

data_folder <- "',paste0(root,'INMET/'),'"

for(VAR in c("rain","temp","rad","RU","WS","WD")){
  output_name <- paste0(output_prefix,"_",VAR,".Rds")
  files <- dir(path = data_folder,pattern = VAR,full.names = T)
  files <- grep(files,pattern = output_prefix,value = TRUE,invert = TRUE)

  obs   <- data.frame(date = as.POSIXct("1666-01-01", tz = "GMT"),stringsAsFactors = F)
  for(i in 1:length(files)){
    cat("opening",files[i],i,"of",length(files),"\\n")
    new   <- readRDS(files[i])
    site  <- new$site[1]
    new   <- new[,c(2,4)]
    names(new) <- c("date",site)
    new <- new[!duplicated(new$date), ]
    obs <- merge(obs, new, by = "date", all = TRUE)
  }
  obs <- obs[-1,]
  cat("OUPUT:",paste0(data_folder,"/",output_name),"\\n")
  saveRDS(object = obs,file = paste0(data_folder,"/",output_name))
}
'),
      file = paste0(root,'merge_INMET.R'),
      append = FALSE)

  cat(paste0('library(eva3dm)

cat("mergin all METAR data\\n")

output_prefix <- "METAR_all_stations"

data_folder <- "',paste0(root,'METAR/'),'"

files <- dir(path = data_folder,pattern = "METAR",full.names = T)
files <- grep(files,pattern = output_prefix,value = TRUE,invert = TRUE)

for(VAR in c("T2","RH","WS","WD","rain")){
  output_name <- paste0(output_prefix,"_",VAR,".Rds")

  obs   <- data.frame(date = as.POSIXct("1666-01-01", tz = "GMT"),stringsAsFactors = F)
  for(i in 1:length(files)){
    cat("opening",files[i],i,"of",length(files),"\\n")

    new   <- readRDS(files[i])
    new   <- new[!duplicated(new$date), ]
    site  <- new$site[1]
    if(!VAR %in% names(new))
      next
    new   <- new[,c("date",VAR)]
    names(new) <- c("date",site)

    obs <- merge(obs, new, by = "date", all = TRUE)
  }
  obs <- obs[-1,]
  cat("OUTPUT:",paste0(data_folder,"/",output_name),"\\n")
  saveRDS(object = obs,file = paste0(data_folder,"/",output_name))
}

'),
      file = paste0(root,'merge_METAR.R'),
      append = FALSE)

  if(verbose){
    cat(' R-Script ', paste0(root,'merge_INMET.R'),': script to merge INMET observations\n')
    cat(' R-Script ', paste0(root,'merge_METAR.R'),': script to merge METAR observations\n')
    cat(' folder ',   paste0(root,'INMET/'),       ': place all INMET data (input and output)\n')
    cat(' folder ',   paste0(root,'METAR/'),       ': place all METAR data (input and output)\n')
  }
}

### script to process meteorological data (.csv) downloaded from ISD
if(template == 'ISD'){
  dir.create(path = paste0(root,'ISD/raw'),
             recursive = TRUE,
             showWarnings = FALSE)
  dir.create(path = paste0(root,'ISD/Rds'),
             recursive = TRUE,
             showWarnings = FALSE)

  cat(paste0('input_folder <- "',paste0(root,'ISD/raw/'),'"
out_folder   <- "',paste0(root,'ISD/Rds/'),'"

calculate_full_humidity <- function(temp_C, dew_point_C, pressure_hPa = 1013.25) {

  # if pressure is missing, consider standard atmosphere
  pressure_hPa[is.na(pressure_hPa)] = 1013.25

  # Constants
  A    <- 17.67
  B    <- 243.5
  RdRv <- 0.622  # Ratio of gas constants for dry air to water vapor

  # Saturation vapor pressure (hPa)
  e_s <- 6.112 * exp((A * temp_C) / (temp_C + B))

  # Actual vapor pressure from dew point (hPa)
  e <- 6.112 * exp((A * dew_point_C) / (dew_point_C + B))

  # Relative Humidity (%)
  RH <- (e / e_s) * 100

  # Absolute Humidity (g/m³)
  AH <- (216.7 * e) / (temp_C + 273.15)

  # Mixing Ratio (w) in g/kg
  w <- RdRv * (e / (pressure_hPa - e)) * 1000  # g/kg

  # Specific Humidity (q) in g/kg
  q <- (w / (1 + (w / 1000)))  # g/kg

  return(list(
    Relative_Humidity_percent = RH,
    Absolute_Humidity_g_m3 = AH,
    Mixing_Ratio_g_per_kg = w,
    Specific_Humidity_g_per_kg = q
  ))
}

input_files  <- dir(path = input_folder, full.names = TRUE)
file_names   <- dir(path = input_folder, full.names = FALSE)
output_files <- paste0(out_folder,"ISD_",substr(file_names,1,11),".Rds")

files <- dir(path = input_folder,pattern = ".csv",full.names = TRUE)

site_list <- data.frame()
for(i in 1:length(files)){
  cat("listing ISD site",i,"from",length(files),"...\\n")
  data      <- read.csv(files[i], nrows = 2)
  station   <- data.frame(ID        = data$STATION[1],
                          name      = data$NAME[1],
                          lon       = data$LONGITUDE[1],
                          lat       = data$LATITUDE[1],
                          elevation = data$ELEVATION[1],
                          type      = data$REPORT_TYPE[1],
                          source    = data$SOURCE[1],
                          stringsAsFactors = FALSE)
  site_list <- rbind(site_list,station)
}
row.names(site_list) <- site_list$ID  # put ID in row.names
site_list            <- site_list[-1] # remove ID

saveRDS(site_list,paste0(out_folder,"site_list_ISD.Rds"))

for(i in 1:length(input_files)){
  cat("processing ISD data",i,"from",length(input_files),"...\\n")
  data    <- read.csv(input_files[i])
  if(nrow(data) < 1) next()
  station <- data.frame(date   = as.POSIXct(data$DATE, format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC"),
                        name   = data$STATION,
                        long   = data$NAME,
                        WS     = as.numeric(substr(data$WND,start = 9,stop = 12))/ 10, ## 0.1 x m/s
                        WD     = as.numeric(substr(data$WND,start = 1,stop = 3)),      ## degree north
                        T2     = as.numeric(substr(data$TMP,start = 1,stop = 5)) / 10, ## 0.1 C
                        DEW    = as.numeric(substr(data$DEW,start = 1,stop = 5)) / 10, ## 0.1 C
                        PRESS  = as.numeric(substr(data$SLP,start = 1,stop = 5)) / 10, ## 0.1 C hpa
                        stringsAsFactors = FALSE)

  station$T2[     station$T2   >= 100   ] = NA
  station$DEW[    station$DEW  >= 100   ] = NA
  station$WS[     station$WS   >= 100   ] = NA
  station$WD[     station$WD   >  360   ] = NA
  station$WS[     station$WS   <= 0     ] = NA
  station$WD[     station$WS   <= 0     ] = NA
  station$PRESS[  station$PRESS >= 9999 ] = NA

  humid <- calculate_full_humidity(temp_C       = station$T2,
                                   dew_point_C  = station$DEW,
                                   pressure_hPa = station$PRESS)

  station$RU <- humid$Relative_Humidity_percent
  station$Q2 <- humid$Specific_Humidity_g_per_kg

  saveRDS(station,paste0(output_files[i]))

  # print( summary(station) )
}
cat("completed!")
'),
      file = paste0(root,'process_isd.R'),
      append = FALSE)

  if(verbose){
    cat(' R-Script ', paste0(root,'process_isd.R'),': script to process meteorological observations and site-list from ISD\n')
    cat(' folder ',   paste0(root,'ISD/raw/'),     ': input folder (.csv)\n')
    cat(' folder ',   paste0(root,'ISD/Rds/'),     ': output folder (.Rds)\n')
  }
}

}
