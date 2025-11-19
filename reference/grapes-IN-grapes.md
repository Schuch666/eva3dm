# Returns the common columns

results of 'd01 in d02' style syntax

## Usage

``` r
x %IN% y
```

## Arguments

- x:

  data.frame

- y:

  data.frame or character string

## Value

data.frame with common columns or a cropped SpatRaster

## Note

A message is always displayed to keep easy to track and debug issues
(with the results and the evaluation process).

Can be used to crop rast objects, such as arguments of sat() function

## See also

See [`select`](https://schuch666.github.io/eva3dm/reference/select.md)
for selection based on time.

## Examples

``` r
times <- seq(as.POSIXct('2024-01-01',tz = 'UTC'),
             as.POSIXct('2024-01-02',tz = 'UTC'),
             by = 'hour')
randon_stuff <- rnorm(25,10)

observation <- data.frame(date   = times,
                          site_1 = randon_stuff,
                          site_3 = randon_stuff,
                          site_4 = randon_stuff,
                          site_5 = randon_stuff,
                          site_6 = randon_stuff,
                          site_7 = randon_stuff)

model_d01 <- data.frame(date  = times,
                        site_1=randon_stuff+1,
                        site_2=randon_stuff+2,
                        site_3=randon_stuff+3,
                        site_4=randon_stuff+4)

model_d02 <- data.frame(date  = times,
                        site_1=randon_stuff-1,
                        site_3=randon_stuff-3)

# multiline
model_d01_in_d02 <- model_d01 %IN% model_d02
#> using model_d01 in model_d02 
eva(mo = model_d01_in_d02, ob = observation, rname = 'd01 in d02')
#> combining all sites...
#> site_1(ok) site_3(ok) ...
#>  total: 2 stations with valid data, 0 stations not included
#> ALL has 50 valid observations
#>             n      Obs      Sim         r      IOA FA2     RMSE MB ME  NMB (%)
#> d01 in d02 50 9.838131 11.83813 0.6844013 0.508409   1 2.236068  2  2 20.32906
#>             NME (%)
#> d01 in d02 20.32906

# or single line
eva(mo = model_d01 %IN% model_d02, ob = observation, rname = 'd01 in d02')
#> using model_d01 in model_d02 
#> combining all sites...
#> site_1(ok) site_3(ok) ...
#>  total: 2 stations with valid data, 0 stations not included
#> ALL has 50 valid observations
#>             n      Obs      Sim         r      IOA FA2     RMSE MB ME  NMB (%)
#> d01 in d02 50 9.838131 11.83813 0.6844013 0.508409   1 2.236068  2  2 20.32906
#>             NME (%)
#> d01 in d02 20.32906
# or
eva(mo = model_d01, ob = observation %IN% model_d02, rname = 'd01 in d02')
#> using observation in model_d02 
#> combining all sites...
#> site_1(ok) site_3(ok) ...
#>  total: 2 stations with valid data, 0 stations not included
#> ALL has 50 valid observations
#>             n      Obs      Sim         r      IOA FA2     RMSE MB ME  NMB (%)
#> d01 in d02 50 9.838131 11.83813 0.6844013 0.508409   1 2.236068  2  2 20.32906
#>             NME (%)
#> d01 in d02 20.32906

```
