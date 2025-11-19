# Calculate daily mean, min or max

function to calculate daily mean, min or max of a data.frame

## Usage

``` r
daily(
  data,
  time = "date",
  stat = mean,
  min_offset = 0,
  hour_offset = 0,
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

  minutes of observation from previous hour (default is 0)

- hour_offset:

  hours of observation from previous day (default is 0)

- numerical:

  TRUE (default) include only numerical columns

- verbose:

  display additional information

## Value

data.frame with time and the daily mean, min or max

## Examples

``` r
# in case there is connection issue
load_data <- function(cond) {
  message(paste("conection issue, loading pre-downloaded data"))
  DATA <- readRDS(paste0(system.file("extdata",package="eva3dm"),
                         "/riem_OAKB_jan_2012.Rds"))
  return(DATA)
}

sites <- c("OAKB")
for(site in sites){
  cat('Trying to download METAR from:',site,'...\n')
  DATA <- tryCatch(riem::riem_measures(station    = sites,
                                       date_start = "2012-01-01",
                                       date_end   = "2012-02-01"),
                   error = load_data)
}
#> Trying to download METAR from: OAKB ...
data_daily_mean <- daily(DATA,time = 'valid')
#> processing daily statistcis ... 
data_daily_min  <- daily(DATA[1:7],time = 'valid',stat = min)
#> processing daily statistcis ... 
data_daily_max  <- daily(DATA[1:7],time = 'valid',stat = max)
#> processing daily statistcis ... 
```
