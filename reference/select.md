# Selection from data.frames with time-series

Utility function to select periods from a data.frame. This function is
inspired by openair::selectByDate.

## Usage

``` r
select(
  data,
  year,
  month,
  day,
  hour,
  minutes,
  seconds,
  julian,
  start,
  end,
  range,
  time = "date"
)
```

## Arguments

- data:

  data.frame with model or observation data

- year:

  numeric vector for selection

- month:

  numeric vector (1-12) for selection, can be abbreviated to 3 or more
  letters

- day:

  numeric vector (1-31) for selection, weekdays can be abbreviated to 3
  or more letters, or weekday/weekend

- hour:

  numeric vector (0-23) for selection

- minutes:

  numeric vector (0-60) for selection

- seconds:

  numeric vector (0-60) for selection

- julian:

  Julian day (1-366)

- start:

  POSIXct or character (YYYY-MM-DD) with the initial date of selection

- end:

  POSIXct or character (YYYY-MM-DD) with the initial date of selection

- range:

  pair of start/end or a data.frame with time (default is "date")

- time:

  name of the column for time (default is "date")

## Value

data.frame

## See also

See
[`%IN%`](https://schuch666.github.io/eva3dm/reference/grapes-IN-grapes.md)
for selection based on position and model domains.

## Examples

``` r
model <- readRDS(paste0(system.file("extdata",package="eva3dm"),
                        "/model.Rds"))
summary(model)
#>       date                       Americana            SAndre      
#>  Min.   :2012-01-01 03:00:00   Min.   : 0.01627   Min.   : 1.590  
#>  1st Qu.:2012-01-03 14:45:00   1st Qu.: 7.88818   1st Qu.: 6.925  
#>  Median :2012-01-06 02:30:00   Median :15.78057   Median :13.457  
#>  Mean   :2012-01-06 02:30:00   Mean   :17.60491   Mean   :15.202  
#>  3rd Qu.:2012-01-08 14:15:00   3rd Qu.:25.95062   3rd Qu.:21.911  
#>  Max.   :2012-01-11 02:00:00   Max.   :48.18681   Max.   :42.690  
#>      VVIbes      
#>  Min.   : 4.747  
#>  1st Qu.: 5.879  
#>  Median : 6.560  
#>  Mean   : 6.944  
#>  3rd Qu.: 7.432  
#>  Max.   :13.101  
summary(select(data = model, start = '2012-01-09'))
#>       date                       Americana          SAndre      
#>  Min.   :2012-01-09 00:00:00   Min.   : 3.747   Min.   : 3.107  
#>  1st Qu.:2012-01-09 12:30:00   1st Qu.: 8.059   1st Qu.: 6.955  
#>  Median :2012-01-10 01:00:00   Median :11.974   Median :12.879  
#>  Mean   :2012-01-10 01:00:00   Mean   :11.406   Mean   :13.981  
#>  3rd Qu.:2012-01-10 13:30:00   3rd Qu.:14.773   3rd Qu.:21.443  
#>  Max.   :2012-01-11 02:00:00   Max.   :19.086   Max.   :25.160  
#>      VVIbes     
#>  Min.   :4.747  
#>  1st Qu.:5.834  
#>  Median :6.236  
#>  Mean   :6.197  
#>  3rd Qu.:6.645  
#>  Max.   :7.078  
summary(select(data = model, start = '2012-01-05', end  = '2012-01-09'))
#>       date              Americana          SAndre           VVIbes      
#>  Min.   :2012-01-05   Min.   : 2.708   Min.   : 4.335   Min.   : 5.477  
#>  1st Qu.:2012-01-06   1st Qu.:11.178   1st Qu.:11.182   1st Qu.: 6.216  
#>  Median :2012-01-07   Median :19.282   Median :15.699   Median : 6.848  
#>  Mean   :2012-01-07   Mean   :20.769   Mean   :19.460   Mean   : 7.169  
#>  3rd Qu.:2012-01-08   3rd Qu.:29.308   3rd Qu.:26.869   3rd Qu.: 7.729  
#>  Max.   :2012-01-09   Max.   :48.187   Max.   :42.690   Max.   :10.365  
summary(select(data = model, day  = 6))
#>       date                       Americana          SAndre      
#>  Min.   :2012-01-06 00:00:00   Min.   : 6.752   Min.   : 4.717  
#>  1st Qu.:2012-01-06 05:45:00   1st Qu.:13.505   1st Qu.:10.566  
#>  Median :2012-01-06 11:30:00   Median :17.723   Median :18.253  
#>  Mean   :2012-01-06 11:30:00   Mean   :18.047   Mean   :21.145  
#>  3rd Qu.:2012-01-06 17:15:00   3rd Qu.:21.546   3rd Qu.:33.314  
#>  Max.   :2012-01-06 23:00:00   Max.   :32.813   Max.   :40.066  
#>      VVIbes     
#>  Min.   :5.770  
#>  1st Qu.:5.888  
#>  Median :6.437  
#>  Mean   :6.379  
#>  3rd Qu.:6.715  
#>  Max.   :7.065  
summary(select(data = model, hour = 12))
#>       date                       Americana         SAndre          VVIbes      
#>  Min.   :2012-01-01 12:00:00   Min.   :12.56   Min.   :15.23   Min.   : 5.812  
#>  1st Qu.:2012-01-03 18:00:00   1st Qu.:16.59   1st Qu.:17.39   1st Qu.: 6.651  
#>  Median :2012-01-06 00:00:00   Median :25.41   Median :20.25   Median : 6.731  
#>  Mean   :2012-01-06 00:00:00   Mean   :24.89   Mean   :21.76   Mean   : 7.182  
#>  3rd Qu.:2012-01-08 06:00:00   3rd Qu.:32.72   3rd Qu.:26.79   3rd Qu.: 7.285  
#>  Max.   :2012-01-10 12:00:00   Max.   :38.08   Max.   :30.54   Max.   :10.191  
summary(select(data = model, day = 6, hour = 12))
#>       date                       Americana         SAndre         VVIbes     
#>  Min.   :2012-01-06 12:00:00   Min.   :15.04   Min.   :30.1   Min.   :6.657  
#>  1st Qu.:2012-01-06 12:00:00   1st Qu.:15.04   1st Qu.:30.1   1st Qu.:6.657  
#>  Median :2012-01-06 12:00:00   Median :15.04   Median :30.1   Median :6.657  
#>  Mean   :2012-01-06 12:00:00   Mean   :15.04   Mean   :30.1   Mean   :6.657  
#>  3rd Qu.:2012-01-06 12:00:00   3rd Qu.:15.04   3rd Qu.:30.1   3rd Qu.:6.657  
#>  Max.   :2012-01-06 12:00:00   Max.   :15.04   Max.   :30.1   Max.   :6.657  
summary(select(data = model, day  = 'weekday'))
#>       date                       Americana            SAndre      
#>  Min.   :2012-01-02 00:00:00   Min.   : 0.01627   Min.   : 1.590  
#>  1st Qu.:2012-01-03 18:30:00   1st Qu.: 8.01003   1st Qu.: 6.112  
#>  Median :2012-01-05 13:00:00   Median :14.70766   Median :13.017  
#>  Mean   :2012-01-06 03:18:56   Mean   :16.78762   Mean   :15.391  
#>  3rd Qu.:2012-01-09 07:30:00   3rd Qu.:25.95556   3rd Qu.:23.190  
#>  Max.   :2012-01-11 02:00:00   Max.   :44.27197   Max.   :42.690  
#>      VVIbes      
#>  Min.   : 4.747  
#>  1st Qu.: 5.861  
#>  Median : 6.585  
#>  Mean   : 7.113  
#>  3rd Qu.: 7.860  
#>  Max.   :13.101  
summary(select(data = model, day  = 'weekend'))
#>       date                       Americana          SAndre      
#>  Min.   :2012-01-01 03:00:00   Min.   : 1.208   Min.   : 1.671  
#>  1st Qu.:2012-01-01 20:00:00   1st Qu.: 7.392   1st Qu.: 8.799  
#>  Median :2012-01-07 13:00:00   Median :21.959   Median :14.428  
#>  Mean   :2012-01-06 00:28:41   Mean   :19.630   Mean   :14.731  
#>  3rd Qu.:2012-01-08 06:00:00   3rd Qu.:25.880   3rd Qu.:16.456  
#>  Max.   :2012-01-08 23:00:00   Max.   :48.187   Max.   :39.050  
#>      VVIbes     
#>  Min.   :5.186  
#>  1st Qu.:5.931  
#>  Median :6.435  
#>  Mean   :6.526  
#>  3rd Qu.:7.024  
#>  Max.   :8.159  
summary(select(data = model, day  = 'tue'))
#>       date                       Americana            SAndre      
#>  Min.   :2012-01-03 00:00:00   Min.   : 0.01627   Min.   : 3.107  
#>  1st Qu.:2012-01-03 11:45:00   1st Qu.: 5.51820   1st Qu.: 4.868  
#>  Median :2012-01-06 23:30:00   Median :12.42998   Median :14.119  
#>  Mean   :2012-01-06 23:30:00   Mean   :13.54270   Mean   :14.208  
#>  3rd Qu.:2012-01-10 11:15:00   3rd Qu.:17.70296   3rd Qu.:23.497  
#>  Max.   :2012-01-10 23:00:00   Max.   :37.74826   Max.   :26.597  
#>      VVIbes     
#>  Min.   :5.396  
#>  1st Qu.:5.828  
#>  Median :6.199  
#>  Mean   :6.514  
#>  3rd Qu.:6.691  
#>  Max.   :8.836  
summary(select(data = model, day  = 'jan'))
#>       date       Americana       SAndre        VVIbes   
#>  Min.   :NA    Min.   : NA   Min.   : NA   Min.   : NA  
#>  1st Qu.:NA    1st Qu.: NA   1st Qu.: NA   1st Qu.: NA  
#>  Median :NA    Median : NA   Median : NA   Median : NA  
#>  Mean   :NaN   Mean   :NaN   Mean   :NaN   Mean   :NaN  
#>  3rd Qu.:NA    3rd Qu.: NA   3rd Qu.: NA   3rd Qu.: NA  
#>  Max.   :NA    Max.   : NA   Max.   : NA   Max.   : NA  
```
