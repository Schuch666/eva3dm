# eval3dmodel

An R-package focusing on evaluation of weather and air quality models. 

The following workflow is recommended:
- *Pre-processing of observations*: download (METAR can be downloded using the R-package [riem](https://docs.ropensci.org/riem/) or this [site](https://mesonet.agron.iastate.edu/request/download.phtml), and Air Quality data for Brazil can be downloaded using [qualR](https://github.com/ropensci/qualR) or [QUALAR](https://qualar.cetesb.sp.gov.br/qualar/home.do) and [MonitorAir](https://www.data.rio/datasets/dados-hor%C3%A1rios-do-monitoramento-da-qualidade-do-ar-monitorar/explore) sites), check the data quality, process data for evaluation, process of site-list if plan to extract time-series from the model.
- *Pre-processing of model output*: extraction and pre-processing
- *Model Evaluation*: `eval()` (to evaluate time-series) or `sate()` (to evaluate against satellite products), available to perform statistical (more details in `stat()`) and categorical (more details in `cate()`) evaluation.
- *Visualization*: try some of the visualization functions from this package or other packages.

The package include:

## Model Post-processing functions:

✔ `extract_serie()` extract and save time-series from WRF outputs and input files (and compatible NetCDF files);

✔ `extract_mean()` extract, average (or max, min, etc) and save variables at surface level;

✔ `extract_8h_max()` extract, calculate maximum (or avarage, max, min) 8h average at surface level;

## NetCDF Utility functions:

✔ `ncdump()` print a `ncdump -h` equivalent command for a NetCDF file;

✔ `vars()` return the name of the variables on NetCDF file;

✔ `atr()` read and write attributes from a Netcdf file;

✔ `wrf_rast()` extract variables and create `SpatRaster` or `SpatVector` from WRF outputs and input files (and compatible NetCDF files);

✔ `rast_to_netcdf()` convert `rast` to an array compatible to a NetCDF WRF file;

## Data pre-processing functions:

✔ `mda8()`, `ma8h()`, `hourly()`, and `daily()` process and calculate calculate time-series;

✔ `rh2q2()`, `q2rh()`, etc.

## Model evaluation functions:

➞ `eval()` data pairing and evaluation against time-series;

✔ `sate()` evaluateion ainst satellite observation;

✔ `stat()` calculate statistical indexes;

✖ `cate()` calculate categorical evaluation;

✔ `write_stat()` and `read_stat()` to write and read evaluation results.

## Visualization:

✔ `plot_rast()` custom plot for `rast` objects;

➞ `plot_diff()` custom plot for absolute or relative difference of `rast` objects;

➞ `overlay()` custom plot to overlay points on `plot_rast()` plot;

✔ `legend_range()` custom legend, display max, min and average;
