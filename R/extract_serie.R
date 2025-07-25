#' Extract time series of wrf file list of lat/lon
#'
#' @description Read and extract data from a list of wrf output files and a table of lat/lon points based on the distance of the points and the center of model grid points, points outside the domain (and points on domain boundary) are not extracted.
#'
#' @param filelist list of files to be read
#' @param point data.frame with lat/lon
#' @param variable variable name
#' @param field '4d' (default), '3d', '2d' or '2dz' see notes
#' @param level model level to be extracted
#' @param prefix to output file, default is serie
#' @param new TRUE, FALSE of 'check' see notes
#' @param return.nearest return the data.frame of nearest points instead of extract the serie
#' @param fast faster calculation of grid distances but less precise
#' @param use_ij logical, use i and j from input instead of calculate
#' @param latitude name of latitude coordinate variable in the netcdf
#' @param longitude name of longitude coordinate variable in the netcdf
#' @param use_TFLAG use the variable TFLAG (CMAQ / smoke) instead of Times (WRF)
#' @param use_datesec use the variable date and datesec (WACCM / CAM-Chem) instead of Times (WRF)
#' @param id name of the column with station names, if point is a SpatVector (points) from terra package
#' @param remove_ch remove special characters on row.names
#' @param verbose display additional information
#'
#' @return No return value
#'
#' @note The field argument '4d' or '2dz' is used to read a 4d/3d variable droping the 3rd dimention (z).
#'
#' @note new = TRUE create a new file, new = FALSE append the data in a old file, and new = 'check' check if the file exist and append if the file exist and create if the file doesnt exist
#'
#' @note FOR CAMx time-series, use the options: use_TFLAG=T, latitude='latitude', longitude='longitude', new=T
#'
#' @note FOR WACCM time-series, use the options: use_datesec=T, latitude='lat', longitude='lon', new=T
#'
#' @note The site-list of two global data-sets (METAR and AERONET) are provided on examples and site-list for stations on Brazil (INMET and Air Quality stations).
#'
#' @import ncdf4 terra
#'
#' @export
#'
#' @examples
#' cat('Example 1: METAR site list\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_METAR.Rds"))
#'
#' cat('Example 2: Integrated Surface Dataset (ISD) site list\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_ISD.Rds"))
#'
#' cat('Example 4: AERONET site list\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AERONET.Rds"))
#'
#' cat('Example 5: list of INMET stations in Brazil\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_INMET.Rds"))
#'
#' cat('Example 6: list of Air Quality stations in Brazil\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))
#'
#' cat('Example 7: list of AQM stations in US\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQS.Rds"))
#'
#' cat('Example 8: list of CASTNET stations in rural areas of US/Canada\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_CASTNET.Rds"))
#'
#' cat('Example 9: list of Longterm European Program (EMEP) stations\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_EMEP.Rds"))
#'
#' cat('Example 10: list of FLUXNET stations\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_FLUXNET.Rds"))
#'
#' cat('Example 11: list of IMPROVE stations\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_IMPROVE.Rds"))
#'
#' cat('Example 12: list of TOAR stations\n')
#' sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_TOAR.Rds"))
#'
#' files    <- dir(path = system.file("extdata",package="eva3dm"),
#'                 pattern = 'wrf.day',
#'                 full.names = TRUE)
#' dir.create(file.path(tempdir(),"SERIE"))
#' folder <- file.path(tempdir(),"SERIE")
#'
#' # extract data for 3 locations
#' extract_serie(filelist = files, point = sites[1:3,],prefix = paste0(folder,'/serie'))
#'

extract_serie <- function(filelist, point, variable = 'o3',field = '4d',level = 1,
                          prefix = 'serie',new = 'check', return.nearest = FALSE,
                          fast = FALSE, use_ij = FALSE,
                          latitude = 'XLAT',longitude = 'XLONG',
                          use_TFLAG = FALSE,use_datesec = FALSE,id = 'id',
                          remove_ch = FALSE,
                          verbose = TRUE){

  if(class(point) %in% 'SpatVector'){
    ge    <- terra::as.data.frame(terra::geom(point))
    new_p <- data.frame(lon = ge[,'x'],
                        lat = ge[,'y'],
                        stringsAsFactors = FALSE)
    new_id <- terra::as.data.frame(point)
    row.names(new_p) <- new_id[,id]
    point  <- new_p
  }

  if(remove_ch)
    row.names(point) <- iconv(row.names(point), from = 'UTF-8', to = 'ASCII//TRANSLIT') # nocov

  output_file  <- paste0(prefix,'.',variable,'.Rds')

  if(new == 'check'){
    new = !file.exists(output_file)
  }

  if(verbose) cat('extracting series of',variable,'field',field,'for',nrow(point),'points\n')

  wrf   <- nc_open(filelist[1])
  lat   <- ncvar_get(wrf,latitude)
  lon   <- ncvar_get(wrf,longitude)
  if(verbose) cat('dim of lat/lon:',dim(lat),'\n')

  if(length(dim(lat)) == 1){   # TEST for WACCM model
    lat   <- matrix(lat, ncol = length(lon),nrow = length(lat), byrow = FALSE) # nocov
    lon   <- matrix(lon, ncol = length(lon),nrow = length(lat), byrow = TRUE)  # nocov
  }

  if(length(dim(lat)) == 3){
    lat   <- lat[,,1,drop = TRUE]   # nocov
    lon   <- lon[,,1,drop = TRUE]   # nocov
  }
  if(length(dim(lat)) == 4){
    lat   <- lat[,,1,1,drop = TRUE] # nocov
    lon   <- lon[,,1,1,drop = TRUE] # nocov
  }
  if(verbose) cat('used dim of lat/lon:',dim(lat),'\n')

  if(use_ij){
    stations <- point
    if(verbose) cat('using i & j to extract points:\n')
  }else{
    nearest <- function(point,lat,lon,fast){
      if(verbose) cat('calculating distances...\n')

      for(i in 1:nrow(point)){
        # OLD CODE
        if(fast){
          d <- ( (lat - point$lat[i])^2 + (lon - point$lon[i])^2 )^(0.5)
        }else{
          d <- lat                # to d get dimmention of lat
          for(k in 1:nrow(d)){    # using NEW CODE
            for(l in 1:ncol(d)){
              d[k,l] <- get_distances(lat1  = point$lat[i],
                                      long1 = point$lon[i],
                                      lat2  = lat[k,l],
                                      long2 = lon[k,l])
            }
          }
        }

        index <- which(d == min(d), arr.ind = TRUE)
        point$i[i] <- index[[1]]
        point$j[i] <- index[[2]]
      }
      return(point)
    }

    max_lat <- max(lat, na.rm = TRUE)
    max_lon <- max(lon, na.rm = TRUE)
    min_lat <- min(lat, na.rm = TRUE)
    min_lon <- min(lon, na.rm = TRUE)
    point   <- point[point$lon >= min_lon,]
    point   <- point[point$lon <= max_lon,]
    point   <- point[point$lat >= min_lat,]
    point   <- point[point$lat <= max_lat,]
    if(verbose) cat('inside lat / lon range:',nrow(point),'points\n')
    if(nrow(point) >= 1){
      stations <- nearest(point,lat,lon,fast)
    }else{                          # in this case all stations are outside the domain
      return('no data to extract!') # nocov
    }
  }

  remove_outsiders <- function(stations,lat){
    j              <- 1
    station_inside <- stations[j,]

    for(i in 1:nrow(stations)){
      outside        = FALSE
      if(stations$i[i] == 1)         outside = TRUE
      if(stations$j[i] == 1)         outside = TRUE
      if(stations$i[i] == nrow(lat)) outside = TRUE
      if(stations$j[i] == ncol(lat)) outside = TRUE

      if(outside){
        if(verbose) cat('* station',rownames(stations)[i],'ouside the domain\n')
      }else{
        #cat('station',rownames(stations)[i],'inside the domain\n')
        station_inside[j,] <- stations[i,]
        rownames(station_inside)[j] <- rownames(stations)[i]
        j <- j + 1
      }
    }
    return(station_inside)
  }
  stations <- remove_outsiders(stations,lat)

  model_lat_lon <- function(point,lat,lon){
    for(i in 1:nrow(point)){
      point$model_lat[i] <- lat[point$i[i],point$j[i]]
      point$model_lon[i] <- lon[point$i[i],point$j[i]]
    }
    return(point)
  }

  stations <- model_lat_lon(stations,lat,lon)

  if(return.nearest){
    return(stations)
  }

  if(verbose){
    print(stations)
    if(level == 1){
      cat('reading',variable,':',filelist[1],'file 1 of',length(filelist),'\n')
    }else{
      cat('reading',variable,':',filelist[1],'file 1 of',length(filelist),paste0('(model level ',level,')'),'\n') # nocov
    }
  }

  if(use_TFLAG){
    TFLAG <- ncvar_get(wrf,'TFLAG')                                      # nocov
    TFLAG <- TFLAG[,1,,drop = TRUE]                                      # nocov
    year  <- as.numeric(substr(x = TFLAG[1,],start = 1,stop = 4))        # nocov
    jday  <- as.numeric(substr(x = TFLAG[1,],start = 5,stop = 7))        # nocov
    day   <- as.Date(paste0(year,jday),format = '%Y%j')                  # nocov
    hour  <- as.numeric(TFLAG[2,]) / 10000                               # nocov
    hour  <- paste(formatC(hour,width = 2, format = "d", flag = "0"))    # nocov
    date_time <- paste0(as.character(day),' ',hour,':00:00')             # nocov
    times     <- as.POSIXlt(date_time, tz = "UTC",                       # nocov
                            format="%Y-%m-%d %H:%M:%OS", optional=FALSE) # nocov
  }else if(use_datesec){
    date      <- ncvar_get(nc = wrf, 'date')                             # nocov
    datesec   <- ncvar_get(nc = wrf, 'datesec')                          # nocov
    year      <- substr(x = date,start = 1,stop = 4)                     # nocov
    month     <- substr(x = date,start = 5,stop = 6)                     # nocov
    day       <- substr(x = date,start = 7,stop = 8)                     # nocov
    date_time <- paste0(year,'-',month,'-',day,' ',                      # nocov
                        datesec/3600,':00:00')                           # nocov
    times     <- as.POSIXct(date_time, tz = "UTC",                       # nocov
                            format="%Y-%m-%d %H:%M:%OS", optional=FALSE) # nocov
  }else{
    TIME   <- ncvar_get(wrf,'Times')
    times  <- as.POSIXlt(TIME, tz = "UTC", format="%Y-%m-%d_%H:%M:%OS", optional=FALSE)
  }

  if(field == '2d')                # 2d Field (x,y)
    contagem  = NA                 # nocov
  if(field == '2dz')               # 3d Field (x,y,z)
    contagem = c(-1,-1,1)          # nocov
  if(field == '3d')                # 3d Field (x,y,t)
    contagem  = NA                 # nocov
  if(field == '4d')
    contagem = c(-1,-1,1,-1)       # 4d Field (x,y,z,t)

  if(field == '2d')                # 2d Field (x,y)
    comeco = NA                    # nocov
  if(field == '2dz')               # 3d Field (x,y,z)
    comeco = c(1,1,level)          # nocov
  if(field == '3d')                # 3d Field (x,y,t)
    comeco  = NA                   # nocov
  if(field == '4d')
    comeco = c(1,1,level,1)        # 4d Field (x,y,z,t)

  var     <- ncvar_get(wrf,variable,count = contagem,start = comeco)
  nc_close(wrf)

  serie <- as.data.frame(times)
  if(length(times) > 1){
    for(i in 1:nrow(stations)){                          # nocov
      serie[,i+1] <- var[stations$i[i],stations$j[i],]   # nocov
    }
  }else{
    for(i in 1:nrow(stations)){
      serie[i+1] <- var[stations$i[i],stations$j[i]]
    }
  }
  names(serie) <- c("date", row.names(stations))

  if(new){
    saveRDS(serie,output_file)
  }else{
    old <- readRDS(output_file)
    saveRDS(rbind(old,serie),output_file)
  }

  if(length(filelist) > 1){
    for(i in 2:length(filelist)){

      if(verbose){
        if(level == 1){
          cat('reading',variable,':',filelist[i],'file',i,'of',length(filelist),'\n')
        }else{
          cat('reading',variable,':',filelist[i],'file',i,'of',length(filelist),paste0('(model level ',level,')'),'\n') # nocov
        }
      }

      wrf   <- nc_open(filelist[i])
      lat   <- ncvar_get(wrf,latitude)
      lon   <- ncvar_get(wrf,longitude)
      if(length(dim(lat)) == 3){
        lat   <- lat[,,1,drop = T]    # nocov
        lon   <- lon[,,1,drop = T]    # nocov
      }
      if(length(dim(lat)) == 4){
        lat   <- lat[,,1,1,drop = T]  # nocov
        lon   <- lon[,,1,1,drop = T]  # nocov
      }

      if(use_TFLAG){
        TFLAG <- ncvar_get(wrf,'TFLAG')                                      # nocov
        TFLAG <- TFLAG[,1,,drop = T]                                         # nocov
        year  <- as.numeric(substr(x = TFLAG[1,],start = 1,stop = 4))        # nocov
        jday  <- as.numeric(substr(x = TFLAG[1,],start = 5,stop = 7))        # nocov
        day   <- as.Date(paste0(year,jday),format = '%Y%j')                  # nocov
        hour  <- as.numeric(TFLAG[2,]) / 10000                               # nocov
        hour  <- paste(formatC(hour,width = 2, format = "d", flag = "0"))    # nocov
        date_time <- paste0(as.character(day),' ',hour,':00:00')             # nocov
        times     <- as.POSIXlt(date_time, tz = "UTC",                       # nocov
                                format="%Y-%m-%d %H:%M:%OS", optional=FALSE) # nocov
      } else if(use_datesec){
        date      <- ncvar_get(nc = wrf, 'date')                             # nocov
        datesec   <- ncvar_get(nc = wrf, 'datesec')                          # nocov
        year      <- substr(x = date,start = 1,stop = 4)                     # nocov
        month     <- substr(x = date,start = 5,stop = 6)                     # nocov
        day       <- substr(x = date,start = 7,stop = 8)                     # nocov
        date_time <- paste0(year,'-',month,'-',day,' ',                      # nocov
                            datesec/3600,':00:00')                           # nocov
        times     <- as.POSIXct(date_time, tz = "UTC",                       # nocov
                                format="%Y-%m-%d %H:%M:%OS", optional=FALSE) # nocov
      } else {
        TIME   <- ncvar_get(wrf,'Times')
        times  <- as.POSIXlt(TIME, tz = "UTC", format="%Y-%m-%d_%H:%M:%OS", optional=FALSE)
      }

      var      <- ncvar_get(wrf,variable,count = contagem, start = comeco)
      nc_close(wrf)

      serie <- as.data.frame(times)
      if(length(times) > 1){
        for(i in 1:nrow(stations)){                           # nocov
          serie[,i+1] <- var[stations$i[i],stations$j[i],]    # nocov
        }
      }else{
        for(i in 1:nrow(stations)){
          serie[i+1] <- var[stations$i[i],stations$j[i]]
        }
      }
      names(serie) <- c("date", row.names(stations))

      old <- readRDS(output_file)
      saveRDS(rbind(old,serie),output_file)
    }
  }

  if(verbose) cat('output:',output_file,'\n')
}
