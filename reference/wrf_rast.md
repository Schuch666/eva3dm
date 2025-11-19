# Creates SpatRaster object from wrf file

Creates a SpatRaster (terra R-package) object from a variable from wrf
file (or another compatible NetCDF)

## Usage

``` r
wrf_rast(
  file = file.choose(),
  name = NA,
  map,
  level = 1,
  times,
  latlon = FALSE,
  method = "bilinear",
  as_polygons = FALSE,
  flip_h = FALSE,
  flip_v = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- file:

  wrf file

- name:

  variable name

- map:

  (optional) file with lat-lon variables and grid information

- level:

  only for 4d data, numeric, default is 1 for surface (include all
  times)

- times:

  only for 4d data, numeric, set to select time instead of levels
  (include all levels)

- latlon:

  logical (default is FALSE), set TRUE project the output to
  "+proj=longlat +datum=WGS84 +no_defs"

- method:

  method passed to terra::projection, default is bilinear

- as_polygons:

  logical, true to return a SpatVector instead of SpatRaster

- flip_h:

  horizontal flip (by rows)

- flip_v:

  vertical flip (by cols)

- verbose:

  display additional information

- ...:

  extra arguments passed to ncdf4::ncvar_get

## Value

SpatRaster object (from terra package)

## Examples

``` r
{

wrf <- paste(system.file("extdata", package = "eva3dm"),
                         "/wrfinput_d01", sep="")

r <- wrf_rast(file=wrf, name='XLAT')

plot_rast(r)
}
```
