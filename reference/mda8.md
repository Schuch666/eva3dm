# Maximum Daily 8-hr Average

function to calculate Ozone Maximum Daily 8-hr Average or 8-hr moving
Average for a data.frame

## Usage

``` r
mda8(data, time = "date", var, verbose = TRUE)
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

## Value

data.frame with time and the maximum daily 8-hr average

## See also

[`ma8h`](https://schuch666.github.io/eva3dm/reference/ma8h.md) for
8-hour Moving Average

## Examples

``` r
model_file <- paste(system.file("extdata", package = "eva3dm"),
                    "/model_o3_ugm3_36km.Rds", sep="")
model      <- readRDS(model_file)
model_mda8 <- mda8(model)
#> processing hourly data ... 
#> processing 8-hour moving average ... 
#> processing daily maximum ... 
model_8h   <- ma8h(model)
#> processing hourly data ... 
#> processing 8-hour moving average ... 
plot(model$date,model$Campinas, pch = 19,
     main = expression(O[3]~~'['*mu*g*m^-3*']'))
points(model_8h$date,model_8h$Campinas, col = 'blue', pch = 19)
points(model_mda8$date + 17*60*60,model_mda8$Campinas,
       col = 'red', pch = 4, cex = 2)
legend('topleft',bty = 'n',
       pch = c(19,19,4),
       legend = c('hourly','8h-mov average','MD8A'),
       col = c('black','blue','red'))
```
