# eval3dmodel

An R-package focusing on evaluation of weather and air quality models.

The package include Post-processing functions:
→ `extract_serie()` extract and save time-series from WRF outputs and input files (and compatible NetCDF files);
→ `extract_mean()` extract, average (or max, min, etc) and save variables at surface level;
→ `extract_8h_max()` extract, calculate maximum (or avarage, max, min) 8h average at surface level;

Utility functions:
→ `ncdump()` print a `ncdump -h` equivalent command for a NetCDF file;
→ `vars()` return the name of the variables on NetCDF file;
→ `atr()` read and write attributes from a Netcdf file;
✔ `wrf_rast()` extract variables and create `SpatRaster` or `SpatVector` from WRF outputs and input files (and compatible NetCDF files);
→ `rast_to_netcdf()` convert `rast` to an array compatible to a NetCDF WRF file;
→ `MDA8()` calculate MAD8 for time-series.
→ `rh2q2()`, `q2rh()`, etc.

Model evaluation functions:
→ `eval()` data pairing and evaluation against time-series;
✔ `stat()` calculate statistical indexes;
✖ `cate()` calculate categorical evaluation;
✔ `sate()` evaluateion ainst satellite observation;
✔ `write_stat()` and `read_stat()` to write and read evaluation results.

Visualization:
✔ `plot_rast()` custom plot for `rast` objects;
→ `plor_diff()` custom plot for absolute or relative difference of `rast` objects;
→ `overlay()` custom plot to overlay points on `plot_rast()` plot;
✔ `legend_range()` custom legend, display max, min and average;
