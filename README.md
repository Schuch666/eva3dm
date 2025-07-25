# eva3dm <img src="man/figures/logo.gif" align="right" width="140"/>

<!-- badges: start -->
[![Licence:MIT](https://img.shields.io/github/license/hyperium/hyper.svg)](https://raw.githubusercontent.com/Schuch666/eva3dm/refs/heads/JOSS/MIT) 
[![codecov](https://codecov.io/github/schuch666/eva3dm/graph/badge.svg?token=E14U1GNK6R)](https://codecov.io/github/schuch666/eva3dm)
[![R build status](https://github.com/schuch666/eva3dm/workflows/R-CMD-check/badge.svg)](https://github.com/schuch666/eva3dm/actions) 
[![Site build status](https://github.com/schuch666/eva3dm/workflows/site/badge.svg)](https://schuch666.github.io/eva3dm/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/eva3dm)](http://cran.r-project.org/web/packages/eva3dm)
[![cran checks](https://badges.cranchecks.info/worst/eva3dm.svg)](https://cran.r-project.org/web/checks/check_results_eva3dm.html)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.07797/status.svg)](https://doi.org/10.21105/joss.07797)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15213216.svg)](https://doi.org/10.5281/zenodo.15213216) 
<!-- badges: end -->

## Introduction

An R-package focusing on **EVA**luation of **3D** weather and air quality **M**odels, streamlining the entire workflow from data preparation to post-processing, statistical analysis, and visualization **[1]**.

Whether you're working with meteorological models like WRF or evaluating air quality simulations (WRF-Chem, CAMx, WACCM, etc), eva3dm provides a comprehensive toolkit to handle observational data, model outputs, and evaluation metrics in an efficient and reproducible manner.
Key Features:
 - Pre-processing of observational and model data
 - Post-processing tools for extracting time-series, calculating derived variables (e.g., accumulated rain, humidity variables and wind variables), and preparing data for analysis
 - Statistical and categorical model evaluation
 - Custom plotting tools for spatial data and georeferencing evaluation results
 - Integration with other packages like `terra`, `ncdf4`, `qualR`, `riem` and `openair`

**[1]** _Schuch, D., (2025). “eva3dm: A R-package for model evaluation of 3D weather and air quality models.” **Journal of Open Source Software**, 10(108), 7797, [doi:10.21105/joss.07797](https://doi.org/10.21105/joss.07797)_

## Instalation

Open a R session and type the following command on the terminal to install the official version from CRAN:

`install.packages("eva3dm")`

the last version can be installed directly from github (using the `remotes` package, in this example):

`remotes::install_github("schuch666/eva3dm")`

All the functions include examples in the [documentation](https://schuch666.github.io/eva3dm/).

## Recomended Workflow
The following workflow is recommended:

**1. Pre-processing of observations**:

- Download of observations, time-series of meteorological variables can be obtained from the METAR (METeorological Aerodrome Report) from Automated Surface Observing System/Automated Weather Observing System (ASOS/AWOS) can be downloaded using the R-package [riem](https://docs.ropensci.org/riem/), or via the [Iowa State University](https://mesonet.agron.iastate.edu/request/download.phtml) website and METAR data from Integrated Surface Database (ISD) can be downloaded using the R-package [worldmet](https://openair-project.github.io/worldmet/) or the [NOAA Global hourly ISD](https://www.ncei.noaa.gov/products/land-based-station/integrated-surface-database) website, time-series of pollutant concentrations from Brazil can be downloaded using the R-package [qualR](https://github.com/ropensci/qualR), or [QUALAR](https://qualar.cetesb.sp.gov.br/qualar) and [MonitorAir](https://www.data.rio/datasets/dados-hor%C3%A1rios-do-monitoramento-da-qualidade-do-ar-monitorar/explore) sites, and a range of satellite products are available at [NASA giovanni](https://giovanni.gsfc.nasa.gov/giovanni/) website.

- QA of the observation data.

- Process observation data for evaluation.

- Process of site-list if plan to extract time-series from the model.

**2. Pre-processing of model output**: Extraction and pre-processing of model outputs;

**3. Model Evaluation**: The functions `eva()` (to evaluate time-series) and `sat()` (to evaluate against satellite products) can be used to perform statistical (more details in `stat()`) and categorical (more details in `cate()`) evaluation;

**4. Visualization**: try some of the visualization functions from this package or other packages.

This package includes:

## Model Post-processing functions:

✔ `extract_serie()` extract and save time-series from WRF outputs and input files (and compatible NetCDF files);

✔ `extract_mean()` extract, average (or max, min, etc) and save variables in a NetCDF file;

✔ `extract_max_8h()` extract, calculate maximum (or avarage, max, min) 8h average and save variables in a NetCDF file;

✔ `calculate_column()` Calculate the column concentration of trace gases from WRF-Chem;

✔ `wrf_rast()` extract variables and create `SpatRaster` or `SpatVector` from WRF files (and compatible NetCDF files) and the contrapart `rast_to_netcdf()` that converts `rast` to an array compatible to a NetCDF WRF file.

## Data pre-processing functions:

✔ `select()` function to select time-series (data.frame) based on time;

✔ `mda8()`, `ma8h()`, `hourly()`, `daily()`, `monthly()`, and `yearly()` process and calculate calculate time-series;

✔ `rh2q()`, `q2rh()`, that convert humidity units;

✔ `uv2ws()`, `uv2wd()`, that convert model wind components into wind speed and velocity;

✔ `rain()` to calculate hourly precipitation from model accumulated precipitation variables.

## Model evaluation functions:

✔ `eva()` data pairing and evaluation for time-series, `%IN%` allows fair evaluation;

✔ `sat()` evaluation for satellite image, `%IN%` can be used for fair evaluation;

✔ `stat()` calculate statistical metrics (integrated in `eva()` and `sat()`);

✔ `cate()` calculate categorical metrics (integrated in `eva()` and `sat()`);

✔ `write_stat()` and `read_stat()` to write and read evaluation results for `eva()` and `sat()`.

## Visualization and Utility functions:

✔ `ncdump()` print a `ncdump -h` equivalent command for a NetCDF file;

✔ `vars()` return the name of the variables on NetCDF file;

✔ `atr()` read and write attributes from a Netcdf file;

✔ `interp()` Interpolation (project and resample);

✔ `plot_rast()` custom plot for terra `SpatRaster` objects;

✔ `plot_diff()` custom plot for absolute or relative difference of terra `SpatRaster` objects;

✔ `overlay()` custom plot to overlay points or plot point-data,`%at%` can be used to georeference the evaluation results;

✔ `legend_range()` custom legend, display max, min and average;

✔ `template()` function that create post-processing and evaluation scripts;

✔ `%at%` combine a table (with results from `eva()` or `sat()` for example) with a table with geographical coordinates. The goal of this function is to georeference and visualize the statistical results;

✔ `%IN%` filter a table (with model/observations time-series) based on a second table (using the column names). Also can be used to crop a `SpatRaster` based on a second `SpatRaster`. The goal is to be used to perform fair comparison of different evaluations (using the same number of observation).

## Contributing to `eva3dm`

If you like to contribute to `eva3dm` take a look at the [contribution guidelines](https://github.com/schuch666/eva3dm/blob/JOSS/CONTRIBUTING.md) page and by participating in this project you agree to abide the [code of conduct](https://github.com/schuch666/eva3dm/blob/JOSS/CODE_OF_CONDUCT.md) terms.
