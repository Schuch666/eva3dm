# Functions to model evaluation using satellite

functions to evaluate the spatial performance using satellite

## Usage

``` r
sat(
  mo,
  ob,
  rname,
  table = NULL,
  n = 6,
  min = NA,
  max = NA,
  scale,
  method = "bilinear",
  eval_function = stat,
  mask,
  skip_interp = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- mo:

  SpatRaster or raster with model

- ob:

  SpatRaster or raster with observations

- rname:

  passed to stat

- table:

  data.frame to append the results

- n:

  number of points from the boundary removed, default is 5

- min:

  minimum value cutoff

- max:

  maximum value cutoff

- scale:

  multiplier for model and observation (after min/max cutoff)

- method:

  passed to terra::resample

- eval_function:

  evaluation function (default is stat)

- mask:

  optional SpatVector to mask the results

- skip_interp:

  skip the interpolation step

- verbose:

  set TRUE to display additional information

- ...:

  other arguments passed to stat

## Value

a data.frame

## Note

If a YOU DIED error message appears, means you are removing all the
valid values using the arguments min or max.

If cate() is used for eval_function, the argument threshold must be
included (see example).

## Examples

``` r
model_no2 <- terra::rast(paste0(system.file("extdata",package="eva3dm"),
                              "/camx_no2.Rds"))
omi_no2   <- terra::rast(paste0(system.file("extdata",package="eva3dm"),
                              "/omi_no2.Rds"))

# generate the statistical indexes
sat(mo = model_no2,ob = omi_no2,rname = 'NO2_statistical')
#> removing 6 points for the model (mo) lateral boundaryes ...
#> interpolating obsservation (ob) to model grid (mo) ...
#>                     n      Obs      Sim         r       IOA       FA2    RMSE
#> NO2_statistical 11664 3.334283 4.267475 0.4689121 0.3358893 0.6685528 4.98183
#>                        MB       ME  NMB (%)  NME (%)
#> NO2_statistical 0.9331919 2.165081 27.98778 64.93393

# generate categorical evaluation using 3.0 as threshold
sat(mo = model_no2,ob = omi_no2,rname = 'NO2_categorical',
    eval_function = cate, threshold = 3.0)
#> removing 6 points for the model (mo) lateral boundaryes ...
#> interpolating obsservation (ob) to model grid (mo) ...
#>                     n      Obs      Sim thr        A      CSI      POD        B
#> NO2_categorical 11664 3.334283 4.267475   3 83.04184 69.81074 85.49533 107.9626
#>                      FAR      HSS      PSS
#> NO2_categorical 20.81025 66.05674 66.45827

# customizing the evaluation function: inclusion of p.value from stats::cor.test()
stat_p <- function(x, y, ...){
  table         <- eva3dm::stat(x, y, ...)
  cor.result    <- stats::cor.test(x, y, ... )
  table$p.value <- cor.result$p.value
  table         <- table[,c(1:4,12,5:11)]
  return(table)
}

sat(mo = model_no2,ob = omi_no2,rname = 'NO2_statistical_with_p',eval_function = stat_p)
#> removing 6 points for the model (mo) lateral boundaryes ...
#> interpolating obsservation (ob) to model grid (mo) ...
#>                            n      Obs      Sim         r p.value       IOA
#> NO2_statistical_with_p 11664 3.334283 4.267475 0.4689121       0 0.3358893
#>                              FA2    RMSE        MB       ME  NMB (%)  NME (%)
#> NO2_statistical_with_p 0.6685528 4.98183 0.9331919 2.165081 27.98778 64.93393
```
