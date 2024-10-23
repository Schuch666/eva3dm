args <- commandArgs(trailingOnly = TRUE)

library(eva3dm)

dir  <- args[1]

var  <- args[2]
if(length(args) > 2){
   ndim <- args[3]
}else{
   ndim <- '4d'
}

if(ndim == '&')
   ndim <- '4d'

sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),
                        "/sites_METAR.Rds"))

files    <- dir(path = paste0('WRF/',dir),
                pattern = "wrfout_d01",full.names = T)

extract_serie(filelist = files,
              new      = T,
              point    = sites,
              variable = var,
              field    = ndim,
              prefix   = "metar.d01",
              fast     = TRUE)

  