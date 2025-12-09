# Create a NetCDF file with the surface mean

Read and calculate the mean value of a variable from a list of wrf
output files.

## Usage

``` r
extract_mean(
  filelist,
  variable = "o3",
  field = "4d",
  prefix = "mean",
  units = "ppmv",
  meta = TRUE,
  filename,
  verbose = TRUE
)
```

## Arguments

- filelist:

  list of files to be read

- variable:

  variable name

- field:

  '4d' (default), '3d', '2d' or '2dz' see notes

- prefix:

  to output file, default is serie

- units:

  units on netcdf file (default is ppmv)

- meta:

  use Times, XLONG and XLAT data (only works with 2d variable for file)

- filename:

  name for the file, in this case prefix is not used

- verbose:

  display additional information

## Value

No return value

## Note

The field argument '4d' / '2dz' is used to read a 4d/3d variable droping
the 3rd dimention (z).

## Examples

``` r
dir.create(file.path(tempdir(), "MEAN"))
folder <- system.file("extdata",package="eva3dm")
wrf_file <- paste0(folder,"/wrf.day1.o3.nc")
extract_mean(filelist = wrf_file,prefix = paste0(file.path(tempdir(),"MEAN"),'/mean'))
#> extracting mean of o3 field 4d 
#> reading: /home/runner/work/_temp/Library/eva3dm/extdata/wrf.day1.o3.nc file 1 of 1 
#> min: 2.123069e-05 mean: 0.007767247 max: 0.03175572 
#> total times: 1 
#> output: /tmp/RtmpUurXnw/MEAN/mean.o3.nc 
```
