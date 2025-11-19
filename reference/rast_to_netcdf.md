# Function to convert/save a SpatRaster array/Netcdf

Conversion of SpatRaster to array and optionally save on a existing
Netcdf File.

## Usage

``` r
rast_to_netcdf(r, file, name, unit = units(r), XY = FALSE, verbose = TRUE)
```

## Arguments

- r:

  SpatRaster object

- file:

  Netcdf file name

- name:

  variable name on a Netcdf file

- unit:

  unit of the variable (set to NA to don't change unit)

- XY:

  set to true if MemoryOrder is XY (only if file is missing)

- verbose:

  display additional information

## Value

numerical array

## Note

eva3dm::wrf_rast support 3d SpatRaster, in case of a 4d variable use
other approach to save on file.

## Examples

``` r
folder   <- system.file("extdata",package="eva3dm")
wrf_file <- paste0(folder,"/wrf.day1.o3.nc")

Rast     <- wrf_rast(wrf_file,'o3')
A        <- rast_to_netcdf(Rast)
```
