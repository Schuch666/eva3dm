# Print a 'ncdump -h' command

Read a NetCDF and print the medatada

## Usage

``` r
ncdump(file = file.choose())
```

## Arguments

- file:

  file name

## Value

No return value, only display information

## Examples

``` r
ncdump(file = paste0(system.file("extdata",package="eva3dm"),
                     '/wrfinput_d01'))
#> File /home/runner/work/_temp/Library/eva3dm/extdata/wrfinput_d01 (NC_FORMAT_CLASSIC):
#> 
#>      3 variables (excluding dimension variables):
#>         char Times[DateStrLen,Time]   
#>         float XLAT[west_east,south_north]   
#>             MemoryOrder: XY
#>             description: LATITUDE, SOUTH IS NEGATIVE
#>             units: degree north
#>             stagger: 
#>             FieldType: 104
#>         float XLONG[west_east,south_north]   
#>             MemoryOrder: XY
#>             description: LONGITUDE, WEST IS NEGATIVE
#>             units: degree east
#>             stagger: 
#>             FieldType: 104
#> 
#>      4 dimensions:
#>         DateStrLen  Size:19 
#>         Time  Size:1   *** is unlimited *** 
#>         west_east  Size:149 
#>         south_north  Size:99 
#> 
#>     79 global attributes:
#>         TITLE:  OUTPUT FROM REAL_EM V3.9.1.1 PREPROCESSOR
#>         START_DATE: 2011-08-01_00:00:00
#>         SIMULATION_START_DATE: 2011-08-01_00:00:00
#>         WEST-EAST_GRID_DIMENSION: 150
#>         SOUTH-NORTH_GRID_DIMENSION: 100
#>         BOTTOM-TOP_GRID_DIMENSION: 35
#>         DX: 9000
#>         DY: 9000
#>         GRIDTYPE: C
#>         DIFF_OPT: 1
#>         KM_OPT: 4
#>         DAMP_OPT: 3
#>         DAMPCOEF: 0.200000002980232
#>         KHDIF: 0
#>         KVDIF: 0
#>         MP_PHYSICS: 10
#>         RA_LW_PHYSICS: 4
#>         RA_SW_PHYSICS: 4
#>         SF_SFCLAY_PHYSICS: 1
#>         SF_SURFACE_PHYSICS: 2
#>         BL_PBL_PHYSICS: 1
#>         CU_PHYSICS: 11
#>         SF_LAKE_PHYSICS: 0
#>         SURFACE_INPUT_SOURCE: 1
#>         SST_UPDATE: 0
#>         GRID_FDDA: 0
#>         GFDDA_INTERVAL_M: 0
#>         GFDDA_END_H: 0
#>         GRID_SFDDA: 0
#>         SGFDDA_INTERVAL_M: 0
#>         SGFDDA_END_H: 0
#>         HYPSOMETRIC_OPT: 2
#>         USE_THETA_M: 0
#>         USE_MAXW_LEVEL: 0
#>         USE_TROP_LEVEL: 0
#>         GWD_OPT: 0
#>         SF_URBAN_PHYSICS: 1
#>         SF_OCEAN_PHYSICS: 0
#>         SIMULATION_INITIALIZATION_TYPE: REAL-DATA CASE
#>         WEST-EAST_PATCH_START_UNSTAG: 1
#>         WEST-EAST_PATCH_END_UNSTAG: 149
#>         WEST-EAST_PATCH_START_STAG: 1
#>         WEST-EAST_PATCH_END_STAG: 150
#>         SOUTH-NORTH_PATCH_START_UNSTAG: 1
#>         SOUTH-NORTH_PATCH_END_UNSTAG: 99
#>         SOUTH-NORTH_PATCH_START_STAG: 1
#>         SOUTH-NORTH_PATCH_END_STAG: 100
#>         BOTTOM-TOP_PATCH_START_UNSTAG: 1
#>         BOTTOM-TOP_PATCH_END_UNSTAG: 34
#>         BOTTOM-TOP_PATCH_START_STAG: 1
#>         BOTTOM-TOP_PATCH_END_STAG: 35
#>         GRID_ID: 1
#>         PARENT_ID: 1
#>         I_PARENT_START: 1
#>         J_PARENT_START: 1
#>         PARENT_GRID_RATIO: 1
#>         DT: 45
#>         CEN_LAT: -23.5499954223633
#>         CEN_LON: -45
#>         TRUELAT1: -23
#>         TRUELAT2: -24
#>         MOAD_CEN_LAT: -23.5499954223633
#>         STAND_LON: -45
#>         POLE_LAT: 90
#>         POLE_LON: 0
#>         GMT: 0
#>         JULYR: 2011
#>         JULDAY: 213
#>         MAP_PROJ: 1
#>         MAP_PROJ_CHAR: Lambert Conformal
#>         MMINLU: MODIFIED_IGBP_MODIS_NOAH
#>         NUM_LAND_CAT: 21
#>         ISWATER: 17
#>         ISLAKE: 21
#>         ISICE: 15
#>         ISURBAN: 13
#>         ISOILWATER: 14
#>         HYBRID_OPT: -1
#>         ETAC: 0
```
