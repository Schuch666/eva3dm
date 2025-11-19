# conversion of model precipitation to hourly precipitation

function that converts model accumulated precipitation to hourly
precipitation.

## Usage

``` r
rain(rainc, rainnc, verbose = TRUE)
```

## Arguments

- rainc:

  data.frame or SpatRaster with RAINC variable

- rainnc:

  data.frame or SpatRaster with RAINNC variable

- verbose:

  set TRUE to display additional information

## Value

data.frame time and the hourly precipitation or SpatRaster hourly
precipitation

## Examples

``` r
times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
             as.POSIXct('2024-01-01 04:00:00',tz = 'UTC'),
             by = 'hour')
RNC   <- data.frame(date = times, aa = c(0.149,0.149,0.149,0.149,0.149))
RNNC  <- data.frame(date = times, aa = c(0.919,1.0,1.1,1.1,2.919))
rain(rainc = RNC, rainnc = RNNC)
#>                  date    aa
#> 1 2024-01-01 00:00:00    NA
#> 2 2024-01-01 01:00:00 0.081
#> 3 2024-01-01 02:00:00 0.100
#> 4 2024-01-01 03:00:00 0.000
#> 5 2024-01-01 04:00:00 1.819
```
