# Create a NetCDF file with the surface maximum of O3

Read the values from o3 and T2, convert o3 to ug m-3 and calculate the
maximum of 8-hour moving avarage from a list of files.

## Usage

``` r
extract_max_8h(
  filelist,
  variable = "o3",
  field = "4d",
  prefix = "max_8h",
  units = "ug m-3",
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

  units on netcdf file (default is ug m-3), change to skip unit
  conversion

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
dir.create(file.path(tempdir(), "MDA8"))
folder <- system.file("extdata",package="eva3dm")
wrf_file <- paste0(folder,"/test_small_o3.nc")
extract_max_8h(filelist = wrf_file,
               prefix = paste0(file.path(tempdir(),"MDA8"),'/mean'),
               field = '3d')
#> extracting 8h max of o3 field 3d units: ug m-3 
#> reading: /home/runner/work/_temp/Library/eva3dm/extdata/test_small_o3.nc file 1 of 1 
#> min: 2.010639e-13 mean: 23.42075 max: 99.85495 
#> total times: 24 
#> output: /tmp/RtmpWjQDAv/MDA8/mean.o3.nc 
```
