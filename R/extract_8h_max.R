#' Create a NetCDF file with the surface maximum of O3
#'
#' @description Read the values from o3 and T2, convert o3 to ug m-3 and calculate the maximum of 8-hour moving avarage from a list of files.
#'
#' @param filelist list of files to be read
#' @param variable variable name
#' @param field '4d' (default), '3d', '2d' or '2dz' see notes
#' @param prefix to output file, default is serie
#' @param units units on netcdf file (default is ug m-3), change to skip unit conversion
#' @param meta use Times, XLONG and XLAT data (only works with 2d variable for file)
#' @param filename name for the file, in this case prefix is not used
#' @param verbose display additional information
#'
#' @return No return value
#'
#' @note The field argument '4d' / '2dz' is used to read a 4d/3d variable droping the 3rd dimention (z).
#'
#' @import ncdf4
#'
#' @export
#'
#' @examples
#' dir.create(file.path(tempdir(), "MDA8"))
#' folder <- system.file("extdata",package="eva3dm")
#' wrf_file <- paste0(folder,"/test_small_o3.nc")
#' extract_max_8h(filelist = wrf_file,
#'                prefix = paste0(file.path(tempdir(),"MDA8"),'/mean'),
#'                field = '3d')
#'

extract_max_8h <- function(filelist, variable = "o3", field = "4d",
                           prefix = "max_8h", units = "ug m-3", meta = TRUE,
                           filename,verbose = TRUE){

  moving_average <- function(x, n = 8) {
   if (n > length(x)) {
      stop("Window size 'n' should be less than or equal to the length of the vector 'x'.") # nocov
    }
   result <- numeric(length(x) - n + 1)
    for (i in 1:(length(x) - n + 1)) {
    result[i] <- mean(x[i:(i + n - 1)])
   }
    return(result)
  }

  if(missing(filename)){
    output_filename   <- paste0(prefix,'.',variable,'.nc')
  }else{
    output_filename   <- filename # nocov
  }

  COMPRESS <- NA

  acu_times <- 0

  if(!meta)
    field = '2d'               # nocov

  if(field == '2d')            # 2d Field (x,y)
    contagem  = NA             # nocov
  if(field == '2dz')           # 3d Field (x,y,z)
    contagem = c(-1,-1,1)      # nocov
  if(field == '3d')            # 3d Field (x,y,t)
    contagem  = NA             #
  if(field == '4d')            # 4d Field (x,y,z,t)
    contagem = c(-1,-1,1,-1)   # nocov

  if(verbose){
    cat('extracting 8h max of',variable,'field',field,'units:',units,'\n')
    cat('reading:',filelist[1],'file 1 of',length(filelist),'\n')
  }

  w     <- nc_open(filename = filelist[1])
  VAR   <- ncvar_get(w,variable,count = contagem)
  if(units == "ug m-3"){
    T2    <- ncvar_get(w,'T2')
    VAR   <- VAR * 10^3*(48)/(0.0805 * T2) # 48 -> O3 molar mass
  }

  mov_av_max <- function(var){
    moving_max <- var[,,1,drop = TRUE]
    if(verbose) cat('min:', min(var,na.rm = TRUE),
                    'mean:',mean(var,na.rm = TRUE),
                    'max:', max(var,na.rm = TRUE),'\n')

    for(i in 1:dim(var)[1]){
      for(j in 1:dim(var)[2]){
        moving_max[i,j] <- max(moving_average(var[i,j,],n = 8),na.rm = TRUE)
        # moving_max[i,j] <- max(terra::roll(var[i,j,],8,mean,'around',na.rm=T),na.rm = T)
      }
    }
    return(moving_max)
  }

  if(meta){
    times <- ncvar_get(w,"Times")
  }
  nc_close(w)

  if(meta){
    acu_times <- length(times)
  }else{
    acu_times <- 1     # nocov
  }

  tmax2 <- function(var,var2){
    t_max <- var                                               # nocov
    for(i in 1:dim(var)[1]){                                   # nocov
      for(j in 1:dim(var)[2]){                                 # nocov
        t_max[i,j] <- max(c(var[i,j],var2[i,j]),na.rm = TRUE)  # nocov
      }
    }
    return(t_max)                                              # nocov
  }

  MAX_8h <- mov_av_max(VAR)

  if(length(filelist) > 1){ # nocov start
    for(i in 2:length(filelist)){
      if(verbose) cat('reading:',filelist[i],'file',i,'of',length(filelist),'\n')
      w    <- nc_open(filename = filelist[i])
      TEMP <- ncvar_get(w,variable,count = contagem)
      if(units == "ug m-3" | units == "ug m^-3"){
        T2    <- ncvar_get(w,'T2')
        TEMP  <- TEMP * 10^3*(48)/(0.0805 * T2) # 48 -> O3 molar mass
      }
      TEMP <- mov_av_max(TEMP)
      MAX_8h  <- tmax2(MAX_8h,TEMP)
      if(meta){
        times <- ncvar_get(w,"Times")
        acu_times = acu_times + length(times)
      }else{
        acu_times = acu_times + 1
      }
      nc_close(w)
    }
  } # nocov end

  # some input
  wrfinput     <- nc_open(filelist[1])
  if(meta){
    input_time   <- ncdf4::ncvar_get(wrfinput, "Times")
    input_time   <- input_time[1] # first time / first file

    input_lat    <- ncdf4::ncvar_get(wrfinput, "XLAT")  # get lat / lon
    input_lon    <- ncdf4::ncvar_get(wrfinput, "XLONG")
    if(length(dim(input_lon)) > 2){
      input_lat    <- input_lat[,,1,drop = TRUE] # drop time from original lat / lon
      input_lon    <- input_lon[,,1,drop = TRUE]
    }

    g_atributos  <- ncdf4::ncatt_get(wrfinput, 0)
    g_atributos  <- c( list(TITLE = paste0('mean ',variable),
                            History = paste("created on",
                                            format(Sys.time(),
                                                   "%Y-%m-%d at %H:%M")),
                            Author = "eva3dm R-Package, Daniel Schuch 2024"),
                       g_atributos[4:length(g_atributos)])

    #dimentions
    west_east <- ncdf4::ncdim_def("west_east",
                                  units = "",
                                  longname = "",
                                  vals = 1:dim(MAX_8h)[1])
    south_north <- ncdf4::ncdim_def("south_north",
                                    units = "",
                                    longname = "",
                                    vals = 1:dim(MAX_8h)[2])
    bottom_top <- ncdf4::ncdim_def("bottom_top",
                                   units = "",
                                   longname = "",
                                   vals = 1)
    DateStrLen          <- ncdf4::ncdim_def("DateStrLen",
                                            units = "",
                                            longname = "",
                                            vals = 1:19)
    Time                <- ncdf4::ncdim_def("Time",
                                            units = "",
                                            longname = "",
                                            vals = 1,
                                            unlim = TRUE)
    # variables
    Times <- ncdf4::ncvar_def(name = "Times",
                              dim = list(DateStrLen,Time),
                              units = "",
                              prec = "char",
                              compression = COMPRESS)
    XLONG <- ncdf4::ncvar_def(name = "XLONG",
                              units = "",
                              dim = list(west_east,south_north),
                              prec = "float",
                              compression = COMPRESS)
    XLAT <- ncdf4::ncvar_def(name = "XLAT" ,
                             units = "",
                             dim = list(west_east, south_north),
                             prec = "float",
                             compression = COMPRESS)

    mean <- ncdf4::ncvar_def(name = variable,
                             units = "",
                             dim = list(west_east,
                                        south_north,
                                        bottom_top,
                                        Time),
                             prec="float",
                             compression = COMPRESS)

    output_file <- nc_create(filename = output_filename,
                             vars = c(list('Times' = Times,
                                           'XLAT'  = XLAT,
                                           'XLONG' = XLONG,
                                           variable = mean)))

    ncdf4::ncvar_def(name = variable,
                     units = "",
                     dim = list(west_east,
                                south_north,
                                bottom_top,
                                Time),
                     prec="float",
                     compression = COMPRESS)

    for(i in 1:length(g_atributos)){
      ncdf4::ncatt_put(output_file,
                       varid = 0,
                       attname = names(g_atributos)[i],
                       attval = g_atributos[[i]])
    }
    # values for the basic variables
    ncdf4::ncvar_put(output_file,
                     "Times",
                     input_time)
    ncdf4::ncvar_put(output_file,
                     "XLONG",
                     input_lon)
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "MemoryOrder",
                     attval = "XY")
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "description",
                     attval = "LONGITUDE, WEST IS NEGATIVE")
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "units",
                     attval = "degree east")
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "stagger",
                     attval = "")
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "FieldType",
                     attval = 104)
    ncdf4::ncvar_put(output_file,
                     "XLAT",
                     input_lat)
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "MemoryOrder",
                     attval = "XY")
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "description",
                     attval = "LATITUDE, SOUTH IS NEGATIVE")
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "units",
                     attval = "degree north")
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "stagger",
                     attval = "")
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "FieldType",
                     attval = 104)
    # to the variable
    ncdf4::ncvar_put(output_file,
                     varid = variable,
                     MAX_8h)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "MemoryOrder",
                     attval = "XYZ")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "description",
                     attval = paste0(variable," max 8 hour average"))
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "units",
                     attval = units)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "stagger",
                     attval = "Z")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "FieldType",
                     attval = 104)
  }else{
    # global attributes
    g_atributos  <- ncdf4::ncatt_get(wrfinput, 0) # nocov start
    g_atributos  <- c( list(TITLE = paste0('max 8 hour average of ',variable),
                            History = paste("created on",
                                            format(Sys.time(),
                                                   "%Y-%m-%d at %H:%M")),
                            Author = "eva3dm R-package, Daniel Schuch 2024"),
                       g_atributos[4:length(g_atributos)])

    #distentions
    west_east <- ncdf4::ncdim_def("west_east",
                                  units = "",
                                  longname = "",
                                  vals = 1:dim(MAX_8h)[1])
    south_north <- ncdf4::ncdim_def("south_north",
                                    units = "",
                                    longname = "",
                                    vals = 1:dim(MAX_8h)[2])
    bottom_top <- ncdf4::ncdim_def("bottom_top",
                                   units = "",
                                   longname = "",
                                   vals = 1)
    DateStrLen          <- ncdf4::ncdim_def("DateStrLen",
                                            units = "",
                                            longname = "",
                                            vals = 1:19)
    Time                <- ncdf4::ncdim_def("Time",
                                            units = "",
                                            longname = "",
                                            vals = 1,
                                            unlim = TRUE)
    # variables
    mean <- ncdf4::ncvar_def(name = variable,
                             units = "",
                             dim = list(west_east,
                                        south_north,
                                        bottom_top,
                                        Time),
                             prec="float",
                             compression = COMPRESS)

    output_file <- nc_create(filename = output_filename,
                             vars = c(list(variable = mean)))

    ncdf4::ncvar_def(name = variable,
                     units = "",
                     dim = list(west_east,
                                south_north,
                                bottom_top,
                                Time),
                     prec="float",
                     compression = COMPRESS)

    for(i in 1:length(g_atributos)){
      ncdf4::ncatt_put(output_file,
                       varid = 0,
                       attname = names(g_atributos)[i],
                       attval = g_atributos[[i]])
    }
    # to the variable
    ncdf4::ncvar_put(output_file,
                     varid = variable,
                     MAX_8h)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "MemoryOrder",
                     attval = "XYZ")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "description",
                     attval = paste0(variable," max 8 hour average"))
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "units",
                     attval = units)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "stagger",
                     attval = "Z")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "FieldType",
                     attval = 104)
    # nocov end
  }

  ncdf4::nc_close(output_file)

  if(verbose){
    cat('total times:',acu_times,'\n')
    cat('output:',output_filename,'\n')
  }

}
