# Read and write attributes on a NetCDF file

Read and write metadata information of a NetCDF files

## Usage

``` r
atr(file = NA, var = "?", att = NA, action = "get", value = NA, verbose = TRUE)
```

## Arguments

- file:

  file name

- var:

  variable name, 0 to global and "?" to show options

- att:

  attribute names (NA for get all attnames)

- action:

  "get" (default), "write" or "print" (return the value) of an attribute

- value:

  value to write

- verbose:

  display additional information

## Value

string with the NetCDF attribute value

## Examples

``` r
nc <- paste0(system.file("extdata",package="eva3dm"),'/wrfinput_d01')
atr(nc,0)
#> global attributes:
#> TITLE START_DATE SIMULATION_START_DATE WEST-EAST_GRID_DIMENSION SOUTH-NORTH_GRID_DIMENSION BOTTOM-TOP_GRID_DIMENSION DX DY GRIDTYPE DIFF_OPT KM_OPT DAMP_OPT DAMPCOEF KHDIF KVDIF MP_PHYSICS RA_LW_PHYSICS RA_SW_PHYSICS SF_SFCLAY_PHYSICS SF_SURFACE_PHYSICS BL_PBL_PHYSICS CU_PHYSICS SF_LAKE_PHYSICS SURFACE_INPUT_SOURCE SST_UPDATE GRID_FDDA GFDDA_INTERVAL_M GFDDA_END_H GRID_SFDDA SGFDDA_INTERVAL_M SGFDDA_END_H HYPSOMETRIC_OPT USE_THETA_M USE_MAXW_LEVEL USE_TROP_LEVEL GWD_OPT SF_URBAN_PHYSICS SF_OCEAN_PHYSICS SIMULATION_INITIALIZATION_TYPE WEST-EAST_PATCH_START_UNSTAG WEST-EAST_PATCH_END_UNSTAG WEST-EAST_PATCH_START_STAG WEST-EAST_PATCH_END_STAG SOUTH-NORTH_PATCH_START_UNSTAG SOUTH-NORTH_PATCH_END_UNSTAG SOUTH-NORTH_PATCH_START_STAG SOUTH-NORTH_PATCH_END_STAG BOTTOM-TOP_PATCH_START_UNSTAG BOTTOM-TOP_PATCH_END_UNSTAG BOTTOM-TOP_PATCH_START_STAG BOTTOM-TOP_PATCH_END_STAG GRID_ID PARENT_ID I_PARENT_START J_PARENT_START PARENT_GRID_RATIO DT CEN_LAT CEN_LON TRUELAT1 TRUELAT2 MOAD_CEN_LAT STAND_LON POLE_LAT POLE_LON GMT JULYR JULDAY MAP_PROJ MAP_PROJ_CHAR MMINLU NUM_LAND_CAT ISWATER ISLAKE ISICE ISURBAN ISOILWATER HYBRID_OPT ETAC
#> NULL
atr(nc,'Times')
#> variable Times attritutes:
#> not found
#> NULL
atr(nc,'XLAT')
#> variable XLAT attritutes:
#> MemoryOrder description units stagger FieldType
#> NULL
atr(nc,'XLONG')
#> variable XLONG attritutes:
#> MemoryOrder description units stagger FieldType
#> NULL

atr(nc,'XLONG','MemoryOrder')
#> XLONG attribute MemoryOrder:
#> XY
#> [1] "XY"
atr(nc,'XLONG','description')
#> XLONG attribute description:
#> LONGITUDE, WEST IS NEGATIVE
#> [1] "LONGITUDE, WEST IS NEGATIVE"
atr(nc,'XLONG','units')
#> XLONG attribute units:
#> degree east
#> [1] "degree east"
atr(nc,'XLONG','stagger')
#> XLONG attribute stagger:
#> empty
#> [1] ""
atr(nc,'XLONG','FieldType')
#> XLONG attribute FieldType:
#> 104
#> [1] 104
```
