# Interpolation (project and resample)

function to project and interpolate rast

## Usage

``` r
interp(x, y, method = "bilinear", mask, verbose = FALSE)
```

## Arguments

- x:

  rast to be interpolated

- y:

  target rast of the interpolation

- method:

  passed to terra::resample

- mask:

  optional SpatVector to mask a region of the data

- verbose:

  display additional information (not used)

## Value

SpatRaster (terra package)

## Examples

``` r
model_o3 <- terra::rast(paste0(system.file("extdata",package="eva3dm"),
                              "/camx_no2.Rds"))
omi_o3   <- terra::rast(paste0(system.file("extdata",package="eva3dm"),
                              "/omi_no2.Rds"))

# interpolate omi O3 column to model grid
omi_o3c_interp_model <- interp(omi_o3,model_o3)

# interpolate model o3 column to omi grid
model_o3c_interp_omi <- interp(omi_o3,model_o3)
```
