# Extract time series of wrf file list of lat/lon

Read and extract data from a list of wrf output files and a table of
lat/lon points based on the distance of the points and the center of
model grid points, points outside the domain (and points on domain
boundary) are not extracted.

## Usage

``` r
extract_serie(
  filelist,
  point,
  variable = "o3",
  field = "4d",
  level = 1,
  prefix = "serie",
  new = "check",
  return.nearest = FALSE,
  fast = FALSE,
  use_ij = FALSE,
  model = "WRF",
  id = "id",
  remove_ch = FALSE,
  verbose = TRUE
)
```

## Arguments

- filelist:

  list of files to be read

- point:

  data.frame with lat/lon

- variable:

  variable name

- field:

  dimension of the variable, '4d' (default), '2dz', '2dz', '2d' or
  'xyzt', 'xyz', 'xyz', and 'xy' see notes

  |           |                |                                                 |
  |-----------|----------------|-------------------------------------------------|
  | **Field** | **Dimensions** | **Example**                                     |
  | 3dt       | xyzt           | WRF dimensions for 3D array with multiple times |
  | 2dt       | xyt            | WRF dimensions for 2D array with multiple times |
  | 2dz       | xyz            | WRF dimensions for 3D array with single time    |
  | 2d        | xy             | WRF dimensions for 2D array with single time    |

- level:

  model level (index) to be extracted

- prefix:

  to output file, default is serie

- new:

  TRUE, FALSE of 'check' see notes

- return.nearest:

  return the data.frame of nearest points instead of extract the serie

- fast:

  faster calculation of grid distances but less precise

- use_ij:

  logical, use i and j from input instead of calculate

- model:

  "WRF" (default), "CAMx" (CAMx, CMAQ and smoke files), and "WACCM"
  (WACCM and CAM-Chem)

- id:

  name of the column with station names, if point is a SpatVector
  (points) from terra package

- remove_ch:

  remove special characters on row.names

- verbose:

  display additional information

## Value

No return value

## Note

The field argument '4d' or '2dz' is used to read a 4d/3d variable
droping the 3rd dimension (z), this should be based how ncdf4 R-package
reads the model output.

new = TRUE create a new file, new = FALSE append the data in a old file,
and new = 'check' check if the file exist and append if the file exist
and create if the file doesnt exist

The option field '3d' was removed, a new option should be used instead
(2dt or 2dz).

The site-list of two global data-sets (METAR/ISD and AERONET) are
provided on examples and site-list for stations on Brazil (INMET and Air
Quality stations). Site-lists for other regions (US, Canada, Europa,
etc) are provided as additional examples.

## Examples

``` r
cat('Example 1: METAR site list\n')
#> Example 1: METAR site list
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_METAR.Rds"))

cat('Example 2: Integrated Surface Dataset (ISD) site list\n')
#> Example 2: Integrated Surface Dataset (ISD) site list
# row.names are a combination of State Code,  County Code  and Site Number (2,3 and 4 digits)
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_ISD.Rds"))

cat('Example 4: AERONET site list\n')
#> Example 4: AERONET site list
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AERONET.Rds"))

cat('Example 5: list of INMET stations in Brazil\n')
#> Example 5: list of INMET stations in Brazil
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_INMET.Rds"))

cat('Example 6: list of Air Quality stations in Brazil\n')
#> Example 6: list of Air Quality stations in Brazil
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))

cat('Example 7: list of AQM stations in US\n')
#> Example 7: list of AQM stations in US
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQS.Rds"))

cat('Example 8: list of CASTNET stations in rural areas of US/Canada\n')
#> Example 8: list of CASTNET stations in rural areas of US/Canada
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_CASTNET.Rds"))

cat('Example 9: list of Longterm European Program (EMEP) stations\n')
#> Example 9: list of Longterm European Program (EMEP) stations
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_EMEP.Rds"))

cat('Example 10: list of FLUXNET stations\n')
#> Example 10: list of FLUXNET stations
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_FLUXNET.Rds"))

cat('Example 11: list of IMPROVE stations\n')
#> Example 11: list of IMPROVE stations
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_IMPROVE.Rds"))

cat('Example 12: list of TOAR stations\n')
#> Example 12: list of TOAR stations
sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_TOAR.Rds"))

files    <- dir(path = system.file("extdata",package="eva3dm"),
                pattern = 'wrf.day',
                full.names = TRUE)
dir.create(file.path(tempdir(),"SERIE"))
folder <- file.path(tempdir(),"SERIE")

# extract data for 3 locations
extract_serie(filelist = files, point = sites[1:3,],prefix = paste0(folder,'/serie'))
#> extracting series of o3 field 4d for 3 points
#> dim of lat/lon: 125 125 
#> used dim of lat/lon: 125 125 
#> inside lat / lon range: 0 points
#> [1] "no data to extract!"
```
