# Convert absolute humidity to relative humidity

function to convert absolute humidity to relative humidity.

## Usage

``` r
q2rh(q, t = 15, p = 101325)
```

## Arguments

- q:

  vector (or data.frame) of absolute humidity (in g/Kg)

- t:

  vector (or data.frame) of temperature (in Celcius)

- p:

  vector (or data.frame) of pressure (in Pa)

## Value

vector or data.frame with time and the relative humidity, units are

## Note

default values are from standard atmosphere (288.15 K (15C) / 101325 Pa)

if rh and temp arguments are data.frame, both need to have the same
number of lines and columns, first column (time column) will be ignored.

## Examples

``` r
# for a single value (or same length vectors)
q2rh(q = 0.0002038, t = 29.3, p = 100800)
#> [1] 80.76917

# using all data.frames
times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
             as.POSIXct('2024-01-02',tz = 'UTC'),
             by = 'hour')[1:5]
q2   <- data.frame(time = times, a = rep(0.0002038,5))
temp <- data.frame(time = times, a = rep(     29.3,5))
pres <- data.frame(time = times, a = rep(   100800,5))
q2rh(q = q2, t = temp, p = pres)
#>                  time        a
#> 1 2024-01-01 00:00:00 80.76917
#> 2 2024-01-01 01:00:00 80.76917
#> 3 2024-01-01 02:00:00 80.76917
#> 4 2024-01-01 03:00:00 80.76917
#> 5 2024-01-01 04:00:00 80.76917

# using data.frame for q and t (p is cte.)
q2rh(q = q2, t = temp, p = 100000)
#>                  time        a
#> 1 2024-01-01 00:00:00 80.12815
#> 2 2024-01-01 01:00:00 80.12815
#> 3 2024-01-01 02:00:00 80.12815
#> 4 2024-01-01 03:00:00 80.12815
#> 5 2024-01-01 04:00:00 80.12815

# using data.frame for q and p (t is cte.)
q2rh(q = q2, t = 26, p = pres)
#>                  time        a
#> 1 2024-01-01 00:00:00 97.96308
#> 2 2024-01-01 01:00:00 97.96308
#> 3 2024-01-01 02:00:00 97.96308
#> 4 2024-01-01 03:00:00 97.96308
#> 5 2024-01-01 04:00:00 97.96308

# using data.frame only for q (p and t are cte.)
q2rh(q = q2, t = 26, p = 100000)
#>                  time       a
#> 1 2024-01-01 00:00:00 97.1856
#> 2 2024-01-01 01:00:00 97.1856
#> 3 2024-01-01 02:00:00 97.1856
#> 4 2024-01-01 03:00:00 97.1856
#> 5 2024-01-01 04:00:00 97.1856
```
