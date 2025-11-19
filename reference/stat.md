# Calculate evaluation statistics from numerical vectors

Calculate statistical indexes (Number of pairs, observation average,
model average, correlation, Index Of Agreement, Factor of 2, Root Mean
Square Error, Mean Bias, Mean error, Normalized Mean Bias, and
Normalized Mean Bias) for model evaluation

## Usage

``` r
stat(
  model,
  observation,
  wd = FALSE,
  cutoff = NA,
  cutoff_NME = NA,
  nobs = 8,
  rname,
  verbose = TRUE
)
```

## Arguments

- model:

  numeric vector with paired model data

- observation:

  numeric vector with paired observation data

- wd:

  logical, set true to apply a rotation on wind direction, see notes

- cutoff:

  (optionally the maximum) valid value for observation

- cutoff_NME:

  (optionally the maximum) valid value for observation for NME, MFB and
  MFE

- nobs:

  minimum number of observations

- rname:

  row name

- verbose:

  display additional information

## Value

data.frame with calculated Number of pairs, observation average, model
average, correlation, Index Of Agreement, Factor of 2, Root Mean Square
Error, Mean Bias, Mean error, Normalized Mean Bias, and Normalized Mean
Bias

## Note

the option wd = TRUE applies a rotation of 360 on model wind direction
to minimize the angular difference.

## References

Emery, C. and Tai., E. 2001. Enhanced Meteorological Modeling and
Performance Evaluation for Two Texas Ozone Episodes.

Monk, K. et al. 2019. Evaluation of Regional Air Quality Models over
Sydney and Australia: Part 1—Meteorological Model Comparison. Atmosphere
10(7), p. 374. doi: 10.3390/atmos10070374.

Ramboll. 2018. PacWest Newport Meteorological Performance Evaluation.

Zhang, Y. et al. 2019. Multiscale Applications of Two Online-Coupled
Meteorology-Chemistry Models during Recent Field Campaigns in Australia,
Part I: Model Description and WRF/Chem-ROMS Evaluation Using Surface and
Satellite Data and Sensitivity to Spatial Grid Resolutions. Atmosphere
10(4), p. 189. doi: 10.3390/atmos10040189.

Emery, C., Liu, Z., Russell, A.G., Odman, M.T., Yarwood, G. and Kumar,
N. 2017. Recommendations on statistics and benchmarks to assess
photochemical model performance. Journal of the Air & Waste Management
Association 67(5), pp. 582–598. doi: 10.1080/10962247.2016.1265027.

Zhai, H., Huang, L., Emery, C., Zhang, X., Wang, Y., Yarwood, G., ... &
Li, L. (2024). Recommendations on benchmarks for photochemical air
quality model applications in China—NO2, SO2, CO and PM10. Atmospheric
Environment, 319, 120290.

## Examples

``` r
model <- 1:100
data  <- model + rnorm(100,0.2)
stat(model = model, observation = data)
#>     n      Obs  Sim         r       IOA FA2     RMSE         MB        ME
#> 1 100 50.69133 50.5 0.9993804 0.9996789   1 1.034765 -0.1913255 0.8206753
#>      NMB (%)  NME (%)
#> 1 -0.3774324 1.618966
```
