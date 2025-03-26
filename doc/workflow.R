## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eva3dm)
library(riem)
library(terra)

## ----metar--------------------------------------------------------------------
start_date  <- "2016-01-01"
end_date    <- "2016-02-01"
sites       <- c("SBGR","SBKP","SBMT","SBSJ","SBSP","SBST","SBTA")

METAR  <- data.frame(date = seq.POSIXt(as.POSIXct(start_date), 
                                       as.POSIXct(end_date), 
                                       by = "hour"))
for(site in sites){
  cat("downloading METAR from:",site,"...\n")

  DATA <- riem_measures(station    = site,
                        date_start = start_date,
                        date_end   = end_date,
                        elev       = FALSE,
                        latlon     = FALSE)
  DATA <- data.frame(date = DATA$valid,
                     T2   = DATA$tmpf)
  names(DATA) <- c('date', site)
  METAR       <- merge(x     = METAR, 
                       y     = DATA, 
                       by    = "date", 
                       all   = T, 
                       sort  = TRUE)
}


## ----check--------------------------------------------------------------------
plot(METAR$date, METAR[,2], ty = 'l',xlab = '',ylab = 'T2', main = 'METAR OBS')
head(METAR)

## ----observation--------------------------------------------------------------

METAR[,-1] <- 5/9 * (METAR[,-1]-32)
METAR      <- hourly(METAR)

plot(METAR$date, METAR[,2], ty = 'l',xlab = '',ylab = 'T2', main = 'METAR OBS')
head(METAR)


## ----site-list----------------------------------------------------------------
site_list <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_METAR.Rds"))
head(site_list)

## ----model--------------------------------------------------------------------
## to extract time-series from WRF-Chem model
## wrf_files <- dir(pattern = "wrfout_d01")
## extract_serie(filelist = wrf_files, point = site_list, variable="T2", prefix="mmodel.d01", field="3d")
model_d01 <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/model.d01.T2.Rds"))

## ----evaluation---------------------------------------------------------------
table <- data.frame()
for(site in sites){
  table <- eva(mo = model_d01, ob = METAR, site = site, table = table)
}
table <- eva(mo = model_d01, ob = METAR, site = 'ALL', table = table)
print(table)

## ----visualize----------------------------------------------------------------
spatial_table <- table %at% site_list
overlay(spatial_table, z = 'MB', main = 'T2 main bias (MB)',expand = 1.6,lim = 0.1)
masp <- terra::vect(paste0(system.file("extdata",package="eva3dm"),"/masp.shp"))
BR   <- terra::vect(paste0(system.file("extdata",package="eva3dm"),"/BR.shp"))
terra::lines(BR)
terra::lines(masp, col = 'gray')
legend_range(spatial_table$MB,y = table["ALL","MB"])

