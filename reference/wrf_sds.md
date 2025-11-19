# Creates SpatRasterDataset object from wrf file

Creates a SpatRasterDataset (terra R-package) object from a variable
from wrf file (or another compatible NetCDF) for all times and levels

## Usage

``` r
wrf_sds(
  file = file.choose(),
  name = NA,
  map,
  latlon = FALSE,
  method = "bilinear",
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

- latlon:

  logical (default is FALSE), set TRUE project the output to
  "+proj=longlat +datum=WGS84 +no_defs"

- method:

  method passed to terra::projection, default is bilinear

- flip_h:

  horizontal flip (by rows)

- flip_v:

  vertical flip (by cols)

- verbose:

  display additional information

- ...:

  extra arguments passed to ncdf4::ncvar_get

## Value

SpatRasterDataset object (from terra package), each time-step is
considered one subdatasets and each layer is one nlyr.

## Note

The convention adopted to select specific times and atmospheric layers
on wrf_sds is sds\[time,layer\] to keep consistence with sds.

## Examples

``` r
file <- paste0(system.file("extdata",package="eva3dm"),"/wrf_4d_o3_Boston.nc")
O34d <- wrf_sds(file,'o3',verbose = TRUE)
#> reading o3 from /home/runner/work/_temp/Library/eva3dm/extdata/wrf_4d_o3_Boston.nc
#> creating SpatRasterDataset for o3 

# selecting one time, keeping multiple layers
O34d[1,]
#> class       : SpatRaster 
#> size        : 15, 15, 11  (nrow, ncol, nlyr)
#> resolution  : 12000, 12000  (x, y)
#> extent      : 702000.3, 882000.3, 564000.8, 744000.8  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=lcc +lat_0=36.7999992370605 +lon_0=-80.8000030517578 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +R=6370000 +units=m +no_defs 
#> source(s)   : memory
#> names       :  o3_level_01, o3_level_02, o3_level_03, o3_level_04, o3_level_05, o3_level_06,  ... 
#> min values  : 2.448919e-05, 0.003329671,  0.02056964,  0.02271221,  0.02671067,  0.03475916,  ... 
#> max values  : 4.445846e-02, 0.044524230,  0.04455488,  0.04457706,  0.04460543,  0.04464806,  ... 
#> unit        : ppmv 
#> time        : 2024-01-01 01:00:00 UTC 

# selecting one layer, keeping multiple times
O34d[,1]
#> class       : SpatRaster 
#> size        : 15, 15, 2  (nrow, ncol, nlyr)
#> resolution  : 12000, 12000  (x, y)
#> extent      : 702000.3, 882000.3, 564000.8, 744000.8  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=lcc +lat_0=36.7999992370605 +lon_0=-80.8000030517578 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +R=6370000 +units=m +no_defs 
#> source(s)   : memory
#> names       :  o3_level_01,  o3_level_01 
#> min values  : 2.448919e-05, 8.437687e-05 
#> max values  : 4.445846e-02, 4.394168e-02 
#> unit        : ppmv 
#> time        : 2024-01-01 01:00:00 to 2024-01-01 02:00:00 UTC (2 steps) 
```
