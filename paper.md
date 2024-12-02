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
    affiliation: 1
affiliations:
 - name: Department of Civil and Environmental Engineering, Northeastern University, United States
   index: 1
date: 25 November 2024
bibliography: paper.bib

---

# Summary

Eva3dm is a package designed to support the evaluation of 3-dimensional physical models (in special, weather and air quality models) against observation data in order to quantify different errors and bias present in the model results.

# Statement of need

Evaluation is the most important step for any model application, it can assure that the model results represent accurately the interest variables. There are currently other tools available in R [@David:2012], Python [@Ladwig:2017] or other languages [@NCAR:2019;@Appel:2011] but none provide the degree of integrated support presented in this package: from the pre-processing of observations and model output, evaluation and visualization.

# Description

In the literature there are many different evaluation criteria depending on the application and variable [@Emery:2001;@Ramboll:2018;@Monk:2019;@Zhang:2019;@Emery:2017;@Zhai:2024]. These criteria varies depending on the goal of the simulation, the variability of the observations, the error associated to the measurements, and how other models available in the literature compared with observations and should be used accordingly to the particular application.

A brief description of the steps to perform a model evaluation is provided bellow:

**1. Pre-processing of observations**: 

- Download of the observations, some examples include: 
  - METAR can be downloaded using the R-package [riem](https://docs.ropensci.org/riem/) or the [Iowa State University](https://mesonet.agron.iastate.edu/request/download.phtml) site
  - AERONET can be downloaded at [AErosol RObotic NETwork](https://aeronet.gsfc.nasa.gov/new_web/data.html) site
  - Air Quality data for Brazil can be downloaded using the R-package [qualR](https://github.com/ropensci/qualR), or [QUALAR](https://qualar.cetesb.sp.gov.br/qualar) and [MonitorAir](https://www.data.rio/datasets/dados-hor%C3%A1rios-do-monitoramento-da-qualidade-do-ar-monitorar/explore) sites
  - Satellite products are available at [NASA giovanni](https://giovanni.gsfc.nasa.gov/giovanni/) website

- Process a list of the location to extract time-series from the model.

- Process observation data for evaluation: Unit conversion, time zone conversion to UTC, and Calculation of secondary variables.

- Quality Assurance of the observation data: check for values outside the valid range, check if the data is available for the time-period and region of the simulation and note any singular event.

**2. Pre-processing of model output**: Extraction and post-processing of model outputs using `evad3m` functions and/or other tools and calculation of secondary variables.

**3. Model Evaluation**: The evaluation consist in pair observations and model results and calculate statistical and/or categorical indexes that can be compared with the literature.

**4. Visualization**: There are functions to visualize model fields (taken into account different projections) and overlay of the statistical results.

To support these steps, following functions are available:

### Pre-processing of observations:

The functions `rh2q` and `q2rh` converts humidity units and the functions `mda8`, `ma8h`, `hourly`, and `daily` can be used to calculate average of time-series. The format used to evaluate time-series is a data.frame, the first column must contain time (in POSIXlt) and one additional column for each different location, satellite data can be read using the function `rast` form terra package.

### Pre-processing of model outputs:

The function `extract_serie` extract and save time-series from model outputs using a data.frame with name (row names) and latitude (column lat) and longitude (column lon), while the functions `extract_mean` and `extract_max_8h` extract the average or the daily maximum of 8-hour moving average and save in a new NetCDF file;

The function `wrf_rast` can be used to read model output and return a `SpatRaster` or `SpatVector` object from the model files and its counterpart `rast_to_netcdf` that converts a `SpatRaster` to an array and/or save to a existing NetCDF file;

The functions `uv2ws` and `uv2wd` can be used to calculate wind speed and velocity from the model wind components (u and v) and the function `rain` can be used to calculate hourly precipitation from model accumulated precipitation variables.

### Model evaluation functions:

There are two high level evaluation functions implemented in the package: `eva`, that does the temporal pairing of both model and observations by station (or combine all data) and the data of time-series and `sat` that interpolate and pair data in regular grids, these functions call the low level evaluation functions `stat` to calculate the statistical metrics or the `cate` to calculate categorical metrics in relation to a threshold value. These result can be written and read using the `write_stat` and `read_stat` functions. 

### Visualization and extractting information functions:

There are functions for visualization, interpolation and to extract information from NetCDF files, Table 1 show a relation of the visualization functions.

| Function name | Description |
| --- | --------- |
| `plot_rast` | Custom plot for `SpatRaster` objects  |
| `plot_diff` | Custom plot for absolute or relative difference of two `SpatRaster` objects     |
| `overlay`   | Custom plot to overlay points |
| `legend_range` | Custom legend that displays max, min and average |
| `interp` | Interpolation function that combines project and resample for `SpatRaster` objects |
| `ncdump` | Print a `ncdump -h` equivalent command for a NetCDF file |
| `vars` | Return the name of the variables on NetCDF file |
| `atr` | Read and write attributes from a Netcdf file |
Table: Visualization, interpolation and information functions.

Figure 1 shows examples of the first 4 functions on Table 1:

![Figure 1 - Example from the viasualization functions.\label{fig:example}](joss_1.png){ width=100% }

### Speciall functions:

| Function name | Description | Objective |
| --- | --------- | --------- |
| `%at%` | Combine a data.frame containing evaluation results and a data.frame containing geographical coordinates (site list) | To georeference and visualize the statistical results |
| `%IN%` | Filter a observation data.frame based on model time-series data.frame. Also can be used to crop a `SpatRaster` based on a second `SpatRaster` | To perform fair comparison of different simulations |
| `template` | Create folders, post-processing and evaluation scripts | To allow quickly process and evaluate multiple variables from one or multiple simulations |
Table: Special functions.

Note that the examples from `eva3dm` are focused on the Weather Research and Forecasting coupled with Chemistry WRF-Chem [@Grell:2005], but but the package can be applied to other models changing some of the arguments from the package functions.

# References
