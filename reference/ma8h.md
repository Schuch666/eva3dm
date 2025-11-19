# Calculate 8-hour moving average

function to calculate Ozone 8-hour moving average for a data.frame

## Usage

``` r
ma8h(data, time = "date", var, verbose = TRUE, ...)
```

## Arguments

- data:

  data.frame with time column and variable columns to be processed

- time:

  name of the time column (default is date) in POSIXct

- var:

  name of the columns to be calculated

- verbose:

  display additional information

- ...:

  parameters passed to hourly

## Value

data.frame with time and the 8-hour moving average

## See also

[`mda8`](https://schuch666.github.io/eva3dm/reference/mda8.md) for
Maximum Daily 8-hour moving average

## Examples

``` r
model_file <- paste(system.file("extdata", package = "eva3dm"),
                                "/model_o3_ugm3_36km.Rds", sep="")
model      <- readRDS(model_file)
model_8h   <- ma8h(model)
#> processing hourly data ... 
#> processing maximum daily 8h average ... 
plot(model$date,model$Campinas, pch = 19,
     main = expression(O[3]~~'['*mu*g*m^-3*']'))
points(model_8h$date,model_8h$Campinas, col = 'blue', pch = 19)
legend('topleft',bty = 'n',
       pch = 19,
       legend = c('hourly','8h-mov average'),
       col = c('black','blue'))
```
