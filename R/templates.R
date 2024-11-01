#' Create templates for model evaluation
#'
#' @description Create templates of code (r-scripts and job submission script) to read, post-process and evaluate model results.
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
#' @export
#'
#' @note Templates types available:\cr
#'  - WRF (model post-process for METAR + INMET)\cr
#'  - WRF-Chem (model post-process for METAR, AQS in Brazil and AERONET)\cr
#'  - IPEN (model post-process for one experimental site including PBL variables)\cr
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
mv metar.d0* inmet.d0* WRF/$dir
'),
file = paste0(root,'post-R_wrf.sh'),
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
 bash ',   paste0(root,'post-R_wrf.sh'),': post processing job script
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
mv metar.d0* aq.d0* pm.d0* WRF/$dir
'),
      file = paste0(root,'post-R_wrfchem.sh'),
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
 bash ',   paste0(root,'post-R_wrfchem.sh'),': post processing job script
 r-script',paste0(root,'extract_metar.R'),': source code to extract metar using eva3dm::extract_serie()
 r-script',paste0(root,'extract_aq.R'),': source code to extract AQ stations from Brazil using eva3dm::extract_serie()
 r-script',paste0(root,'extract_pm.R'),': source code to extract PM compositon from AERONET sites using eva3dm::extract_serie()\n')
}


### SETUP for an experiment (PM composition MET / CHEM and PBL variables)
if(template == 'IPEN'){
  dir.create(path = paste0(root,'WRF/',case),
             recursive = T,
             showWarnings = F)

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
Rscript extract_epx.R $dir RAINC  3d &

wait

echo $dir

date

tar -cvf Rds_${dir}.tar *.Rds
mv exp.d0* WRF/$dir
'),
      file = paste0(root,'post-R_exp.sh'),
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
append = F)

  if(verbose)
    cat(' folder ',paste0(root,'WRF/',case),': link wrf output files here!
 bash ',   paste0(root,'post-R_exp.sh'),': post processing job script
 r-script',paste0(root,'extract_expr.R'),': source code to extract metar using eva3dm::extract_serie()
 new locations can be added to the data.frame with the list of sites.\n')
}


### SCRIPT TO DOWNLOAD METAR
if(template == 'METAR'){
  dir.create(path = paste0(root,'METAR'),
             recursive = T,
             showWarnings = F)

  cat('library("eva3dm")
library("riem")

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
  cat("downloading METAR from:",site,"...\\n")

  DATA <- riem_measures(station    = site,
                        date_start = start_date,
                        date_end   = end_date)

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

  ',
file = paste0(root,'download_METAR.R'),
append = F)

  if(verbose)
    cat(' folder ',paste0(root),': copy wrfinput_d01 gere!
 folder ',paste0(root,'METAR'),': destination folder
 r-script',paste0(root,'download_METAR.R'),': script that download metar data using riem package and information from eva3dm and wrfinput_d01 file\n')
}

### SCRIPT TO EVALUATION USING METAR
if(template == 'MET'){

  dir.create(path = paste0(root,"WRF",case),
             recursive = T,
             showWarnings = F)

  cat(paste0('library(eva3dm)

setwd("',root,'")

WRF_folder   = "WRF"
METAR_folder = "METAR"
# case         = "WRF-Chem"
case         = "WRF-only"

source("table_metar_T2.R")
source("table_metar_Q2.R")
source("table_metar_Wind.R")

  '),
file = paste0(root,'all_tables.R'),
append = F)


  cat('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "WRF-Chem"

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
mod_stats_d01   <- eva(model_d01,observed,"ALL",table = mod_stats_d01)
cat("...\\n")
print(tail(mod_stats_d01))
cat("\\n")

write_stat(stat = mod_stats_d01,
           file = paste0(WRF_folder,"/",case,"/stats.metar.T2.d01.csv"))


  ',
      file = paste0(root,'table_metar_T2.R'),
      append = F)

  cat('# library(eva3dm)
#
# WRF_folder   = "WRF"
# METAR_folder = "METAR"
#
# case         = "WRF-chem"

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

  ',
file = paste0(root,'table_metar_Q2.R'),
append = F)

  if(verbose)
    cat(' r-script',paste0(root,'all_tables.R'),': setup and run script
 r-script',paste0(root,'table_metar_T2.R'),': evaluation of T2 using METAR
 r-script',paste0(root,'table_metar_Q2.R'),': evaluation of Q2 using METAR\n')
}


}
