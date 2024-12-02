---
title: 'eva3dm: A R-package for model evaluation of 3D weather and air quality models'
tags:
  - R
  - Model Evaluation
  - WRF
  - WRF-Chem
authors:
  - name: Daniel Schuch
    orcid: 0000-0001-5977-4519
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Department of Civil and Environmental Engineering, Northeastern University, United States
   index: 1
date: 25 November 2024
bibliography: paper.bib

---

# Summary

Eva3dm is a package designed to support the evaluation of 3-dimensional physical models (in special, weather and air quality models) against observation data in order to quantify different errors and bias present in the model result and/or classify the model performance based on criteria from the literature.

In the literature there are many different evaluation criteria depending on the application and variable [@Emery:2001;@Ramboll:2018;@Monk:2019;@Zhang:2019;@Emery:2017;@Zhai:2024]. These criteria varies depending on the goal of the simulation, the variability of the observations, the error associated to the measurements, and how other models available in the literature compared with observations and should be used accordingly to the particular application, study region and other uncertainties present on the model and model inputs.

Most of the examples from eva3dm are focused on the Weather Research and Forecasting (WRF) model [@Skamarock:2019] and WRF coupled with Chemistry WRF-Chem [@Grell:2005], but other models, such as, the Comprehensive Air Quality Model with Extensions-CAMx [@ENVIRON:2024], Whole Atmosphere Community Climate Model-WACCM [@Gettelman:2019], and other models can be evaluated changing some of the options from default.

# Statement of need

Evaluation is the most important step for any model application, it can not only assure that the model results represent (or not) accurately the interest variable but also can guide to choose between different model configurations and guide model development. There are currently other tools available in R [@David:2012], Python [@Ladwig:2017] or other languages [@NCAR:2019;@Appel:2011] but none provide the degree of integrated support presented in this package: from the pre-processing of observations and model output, evaluation and visualization.

# Description

The package was designed to support the pre-processing of observations, post-processing of model output, evaluation and visualization of the results from the evaluation. 

**1. Pre-processing of observations**: 

- Download of the observations, example of observations available: 
  - METAR can be downloaded using the R-package [riem](https://docs.ropensci.org/riem/) or via the [Iowa State University](https://mesonet.agron.iastate.edu/request/download.phtml) site;
  - AERONET (Aerossol Optical Depth) can be downloaded at [AErosol RObotic NETwork](https://aeronet.gsfc.nasa.gov/new_web/data.html); 
  - Air Quality data for Brazil can be downloaded using the R-package [qualR](https://github.com/ropensci/qualR), or [QUALAR](https://qualar.cetesb.sp.gov.br/qualar) and [MonitorAir](https://www.data.rio/datasets/dados-hor%C3%A1rios-do-monitoramento-da-qualidade-do-ar-monitorar/explore) sites; 
  - Satellite products are available at [NASA giovanni](https://giovanni.gsfc.nasa.gov/giovanni/) website.

- Process a list of the location to extract time-series from the model. NOTE: There are examples of site-list in the documentation of `extract_serie()` (METAR, AERONET, INMET in Brazil, Air Quality stations in Brazil).

- Process observation data for evaluation: Calculation of secondary variables (absolute humidity for example).

- Quality Assurance of the observation data: in this point the user should check for values that are outside the range of each variable (for example negative concentration and other potential problematic values), check if there is actual data from the time-period and region of the simulation and note any singular event that can be represented.

**2. Pre-processing of model output**: Extraction and post-processing of model outputs using `evad3m` functions and/or other tools;

**3. Model Evaluation**: The function `eva()` is used to evaluate time-series (from automatic meteorological stations at surface, for example) and `sat()` is used to evaluate against data in a regular grid (for example satellite products), for both functions there are two types of evaluation available: statistical (more details in `stat()`) and categorical (more details in `cate()`);

**4. Visualization**: there are functions to visualize model fields (taken into account different projections) and overlay of the statistical results.

The R-package includes:

### Pre-processing of observations:

The functions `rh2q()` and `q2rh()` converts humidity from units and the function `mda8()`, `ma8h()`, `hourly()`, and `daily()` can be used to calculate time-series from observations and model. Most of the processing should be done by the user since the formats and conventions are different depending on the source of the data. The format used to evaluate time-series is a data.frame, the first column must contain time (in POSIXlt) and one additional column for each different location (that can contain `NA` for missing data).

### Pre-processing of model outputs:

The function `extract_serie()` extract and save time-series from model outputs (or other compatible NetCDF files, more details in the documentation) using a data.frame with name (row name) and latitude (column lat) and longitude (column lon), while the functions `extract_mean()` and `extract_max_8h()` extract the average or the daily maximum of 8-hour moving average and save in a new NetCDF file;

The function `wrf_rast()` can be used to read model output and return a `SpatRaster` or `SpatVector` from the model files (and compatible NetCDF files) and and its counterpart `rast_to_netcdf()` that converts to an array and/or save to a existing NetCDF WRF file;

The functions `uv2ws()` and `uv2wd()` can be used to convert the model wind components (u and v) into wind speed and velocity and the function `rain()` can be used to calculate hourly precipitation from model accumulated precipitation variables.

### Model evaluation functions:

There are two high level functions implemented in the package: `eva()`, that does the temporal pairing of both model and observations by station (or combine all data) and the data of time-series and `sat()` that interpolate and pair data in regular grids (it removes points from the boundary by default), these functions call the low level functions `stat()` to calculate the statistical metrics or the `cate()` to calculate categorical metrics. These result can be written and read using the `write_stat()` and `read_stat()` functions. 

### Visualization and extractting information functions:

There are functions for visualization, interpolation and to extract information from NetCDF files:

 - `plot_rast()` custom plot for `SpatRaster` objects;

 - `plot_diff()` custom plot for absolute or relative difference of two `SpatRaster` objects;

 - `overlay()` custom plot to overlay points;

 - `legend_range()` custom legend that displays max, min and average;

 - `interp()` Interpolation function that combines project and resample for `SpatRaster` objects;
 
 - `ncdump()` print a `ncdump -h` equivalent command for a NetCDF file;

 - `vars()` return the name of the variables on NetCDF file;

 - `atr()` read and write attributes from a Netcdf file;

### Speciall functions:

There are tree special functions that make specific tasks:

 - `template()` create folders, post-processing and evaluation scripts. The goal is to allow quickly process or evaluate one or multiple simulations (see the documentation for more details), there are functions to download data, process observations and many different templates to post-process model output and different types of evaluation for different variables.

 - `%at%` combine a table with evaluation results and a table with geographical coordinates. The goal of this function is to georeference and visualize the statistical results. It can be combined with `overlay()` function.

 - `%IN%` filter a table (with model/observations time-series) based on a second table (using the column names). Also can be used to crop a `SpatRaster` based on a second `SpatRaster`. The goal is to be used to perform fair comparison of different evaluations (using the same number of observation) to compare different simulations (with different domains for example) it can be combined with `eva()` or `sat()` functions.

# References

