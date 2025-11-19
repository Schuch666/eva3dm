# Extract time series from a list model files using time UTC, latitude, longitude, and altitude (meters).

Read output from a list of model (WRF or UFS) and calculate the column
of trace gases.

## Usage

``` r
extract_surgical(
  filelist,
  point,
  vars,
  model = "WRF",
  tol = 60,
  boundary = 5,
  include_indices = TRUE,
  include_model = TRUE,
  include_distances = TRUE,
  filename,
  verbose = TRUE
)
```

## Arguments

- filelist:

  list of files to be read

- point:

  data.frame with time (POSIXct in UTC), lat (numeric in degrees), lon
  (numeric in degrees), alt (numeric in meters)

- vars:

  variable names

- model:

  'WRF' (default) or 'UFS'

- tol:

  tolerance in seconds

- boundary:

  number of grid points that are considered

- include_indices:

  to include t,i,j,k indices

- include_model:

  to include model time, latitude, longitude and altitude

- include_distances:

  include the distance from the center of the grid to the observations

- filename:

  name of a file to save the dataframe

- verbose:

  display additional information

## Value

data.frame
