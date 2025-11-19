# Model statistical evaluation

Statistical (or categorical) evaluation from 2 data.frames. The input
data.frames (model and observation) must contain a time column
(containing POSIXlt). The function perform pre-conditioning of the data,
data pairing (using the time column) of the observations and model and
calculate the metrics for statistical or categorical evaluation.

## Usage

``` r
eva(
  mo,
  ob,
  rname = site,
  table = NULL,
  site = "ALL",
  wd = FALSE,
  fair = NULL,
  cutoff = NA,
  cutoff_NME = NA,
  no_tz = FALSE,
  nobs = 8,
  eval_function = stat,
  select_time,
  time = "date",
  remove_ch = FALSE,
  clean = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- mo:

  data.frame with model data

- ob:

  data.frame with observation data

- rname:

  row name of the output (default is site argument)

- table:

  data.frame to append the results

- site:

  name of the station or "ALL" (default) or "complete", see notes

- wd:

  default is FALSE, see notes

- fair:

  model data.frame (or list of names) to perform a fair comparison, see
  notes

- cutoff:

  minimum (optionally the maximum) valid value for observation

- cutoff_NME:

  minimum (optionally the maximum) valid value for observation for NME

- no_tz:

  ignore tz from input (force GMT)

- nobs:

  minimum number of valid observations, default is 8

- eval_function:

  evaluation function (default is stat)

- select_time:

  select the observation (ob) using time from model (mo) data.frame

- time:

  name of the time column (containing time in POSIXct)

- remove_ch:

  remove special characters on column names

- clean:

  remove rows when number of observations \< nobs, for site="complete"

- verbose:

  display additional information

- ...:

  arguments to be passing to stats and plot

## Value

data.frame with statistical metric

## Note

Fair argument can be a data.frame or a character string to be used for
the analysis, alternatively the function

If wd = TRUE, used for wind direction, a rotation of 360 (or -360) is
applied to minimize the wind direction difference.

If site == 'ALL' (default) all the columns from observations are
combined in one column (same for observation) and all the columns are
evaluated together.

If site == 'complete' a internal loop, calls recursively eva() to
evaluate all sites in the first argument (model) and using all sites
(see "ALL").

Special thanks to Kiarash and Libo to help to test the wind direction
option.

## See also

[`stat`](https://schuch666.github.io/eva3dm/reference/stat.md) for
additional information about the statistical metrics and
[`cate`](https://schuch666.github.io/eva3dm/reference/cate.md) for
categorical metrics, and check the example with the custom evaluation
function (inclusion of p.value from stats::cor.test()).

## Examples

``` r
model <- readRDS(paste0(system.file("extdata",package="eva3dm"),
                        "/model.Rds"))
obs   <- readRDS(paste0(system.file("extdata",package="eva3dm"),
                        "/obs.Rds"))

# if there is no observed data
# the function return an empty row
table <- eva(mo = model, ob = obs, site = "VVIbes")
#> VVIbes has only 0 valid observations (lesser than 8 obs)
print(table)
#>        n Obs Sim  r IOA FA2 RMSE MB ME NMB (%) NME (%)
#> VVIbes 0  NA  NA NA  NA  NA   NA NA NA      NA      NA

# if the site are not in the input data frame a message is displayed
# and the function return an empty row
table <- eva(mo = model, ob = obs, site = "Ibirapuera")
#> Ibirapuera not found in observation input
print(table)
#>            n Obs Sim  r IOA FA2 RMSE MB ME NMB (%) NME (%)
#> Ibirapuera 0  NA  NA NA  NA  NA   NA NA NA      NA      NA

# calculating statistical with a few observed values
table <- eva(mo = model, ob = obs, site = "Americana")
#> Americana has 227 valid observations
print(table)
#>             n      Obs      Sim         r       IOA       FA2     RMSE
#> Americana 227 40.92952 18.18197 0.6492022 0.5934335 0.4229075 32.47269
#>                  MB       ME   NMB (%)  NME (%)
#> Americana -22.74754 24.10165 -55.57735 58.88575

# calculating categorical (using 2 for threshold) with a few observed values
table <- eva(mo = model, ob = obs, site = "Americana",
             eval_function = cate, threshold = 2)
#> Americana has 227 valid observations
print(table)
#>             n      Obs      Sim thr        A      CSI      POD        B
#> Americana 227 40.92952 18.18197   2 93.39207 93.36283 97.23502 101.3825
#>                FAR     HSS      PSS
#> Americana 4.090909 8.44313 7.235023

# calculating categorical (using 10 for threshold) with a few observed values
table <- eva(mo = model, ob = obs, site = "Americana",
             eval_function = cate, threshold = 10)
#> Americana has 227 valid observations
print(table)
#>             n      Obs      Sim thr        A      CSI      POD        B   FAR
#> Americana 227 40.92952 18.18197  10 74.00881 71.90476 75.12438 79.60199 5.625
#>                HSS      PSS
#> Americana 24.01997 40.50899

# customizing the evaluation function: inclusion of p.value from stats::cor.test()
stat_p <- function(x, y, ...){
  table         <- eva3dm::stat(x, y, ...)
  cor.result    <- stats::cor.test(x, y, ... )
  table$p.value <- cor.result$p.value
  table         <- table[,c(1:4,12,5:11)]
  return(table)
}

table <- eva(mo = model, ob = obs, site = "Americana",eval_function = stat_p)
#> Americana has 227 valid observations
print(table)
#>             n      Obs      Sim         r      p.value       IOA       FA2
#> Americana 227 40.92952 18.18197 0.6492022 1.488004e-28 0.5934335 0.4229075
#>               RMSE        MB       ME   NMB (%)  NME (%)
#> Americana 32.47269 -22.74754 24.10165 -55.57735 58.88575
```
