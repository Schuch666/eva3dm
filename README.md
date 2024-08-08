# eva3dm <img src="man/figures/logo.gif" align="right" alt="" width="140" />
<!-- badges: start -->
[![Licence:MIT](https://img.shields.io/github/license/hyperium/hyper.svg)](https://opensource.org/licenses/MIT) 
[![R build status](https://github.com/schuch666/eva3dm/workflows/R-CMD-check/badge.svg)](https://github.com/schuch666/eva3dm/actions)
<!-- badges: end -->  

An R-package focusing on **EVA**luation of **3D** weather and air quality **M**odels. 

The following workflow is recommended:
- *Pre-processing of observations*: download (METAR can be downloded using the R-package [riem](https://docs.ropensci.org/riem/) or [Iowa State University](https://mesonet.agron.iastate.edu/request/download.phtml) site, and Air Quality data for Brazil can be downloaded using the R-package [qualR](https://github.com/ropensci/qualR), or [QUALAR](https://qualar.cetesb.sp.gov.br/qualar/home.do) and [MonitorAir](https://www.data.rio/datasets/dados-hor%C3%A1rios-do-monitoramento-da-qualidade-do-ar-monitorar/explore) sites), QA of the observation data, process observation data for evaluation, process of site-list if plan to extract time-series from the model; A range of satellite products are available at [NASA giovanni](https://giovanni.gsfc.nasa.gov/giovanni/) website; 
- *Pre-processing of model output*: extraction and pre-processing of model outputs;
- *Model Evaluation*: `eva()` (to evaluate time-series) or `sat()` (to evaluate against satellite products), available to perform statistical (more details in `stat()`) and categorical (more details in `cate()`) evaluation;
- *Visualization*: try some of the visualization functions from this package or other packages.

The package include:

## Model Post-processing functions:

✔ `extract_serie()` extract and save time-series from WRF outputs and input files (and compatible NetCDF files);

✔ `extract_mean()` extract, average (or max, min, etc) and save variables in a NetCDF file;

✔ `extract_max_8h()` extract, calculate maximum (or avarage, max, min) 8h average and save variables in a NetCDF file;

✔ `wrf_rast()` extract variables and create `SpatRaster` or `SpatVector` from WRF files (and compatible NetCDF files) and the contrapart `rast_to_netcdf()` that converts `rast` to an array compatible to a NetCDF WRF file;

## Data pre-processing functions:

✔ `mda8()`, `ma8h()`, `hourly()`, and `daily()` process and calculate calculate time-series;

✔ `rh2q()`, `q2rh()`, that convert humidity units.

## Model evaluation functions:

✔ `eva()` data pairing and evaluation for time-series;

✔ `sat()` evaluation for satellite image;

✔ `stat()` calculate statistical metrics (integrated in `eva()` and `sat()`);

✔ `cate()` calculate categorical metrics (integrated in `eva()` and `sat()`);

✔ `%IN%` allows fair evaluation on function-call level for `eva()` and `sat()`;

✔ `write_stat()` and `read_stat()` to write and read evaluation results for `eva()` and `sat()`.

## Visualization and Utility functions:

✔ `ncdump()` print a `ncdump -h` equivalent command for a NetCDF file;

✔ `vars()` return the name of the variables on NetCDF file;

✔ `atr()` read and write attributes from a Netcdf file;

✔ `interp()` Interpolation (project and resample);

✔ `plot_rast()` custom plot for terra `SpatRaster` objects;

✔ `plot_diff()` custom plot for absolute or relative difference of terra `SpatRaster` objects;

✔ `overlay()` custom plot to overlay points or plot point-data,`%IN%` can be used to georeference the evaluation results;

✔ `legend_range()` custom legend, display max, min and average;
