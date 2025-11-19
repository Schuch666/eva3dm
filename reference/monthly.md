# Calculate monthly mean, min or max

function to calculate monthly mean, min or max of a data.frame

## Usage

``` r
monthly(
  data,
  time = "date",
  stat = mean,
  min_offset = 0,
  hour_offset = 0,
  days_offset = 0,
  numerical = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  data.frame with time column and variable columns to be processed

- time:

  name of the time column (default is date) in POSIXct

- stat:

  function of the statistics to calculate (default is mean)

- min_offset:

  minutes of observation from previous month (default is 0)

- hour_offset:

  hours of observation from previous month (default is 0)

- days_offset:

  day of observation from previous month (default is 0)

- numerical:

  TRUE (default) include only numerical columns

- verbose:

  display additional information

## Value

data.frame with time and the monthly mean, min or max

## Examples

``` r
times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
             as.POSIXct('2024-12-31',tz = 'UTC'),
             by = 'hour')

DATA <- data.frame(date = times,
                   var1 = rnorm(n = length(times), mean = 1,sd = 1),
                   var2 = rnorm(n = length(times), mean = 2,sd = 0.5),
                   var3 = rnorm(n = length(times), mean = 3,sd = 0.25))

data_month_mean <- monthly(DATA)
#> processing monthly statistcis ... 
data_month_min  <- monthly(DATA,stat = min)
#> processing monthly statistcis ... 
data_month_max  <- monthly(DATA,stat = max)
#> processing monthly statistcis ... 
```
