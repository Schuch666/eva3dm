# Plot a legend with the range of values

Plot a legend with the range of values

## Usage

``` r
legend_range(
  x,
  y,
  text.width = NULL,
  dig = c(2, 2, 2),
  xjust = 0.005,
  yjust = 0.95,
  horiz = TRUE,
  y.intersp = 0.5,
  x.intersp = 0.5,
  show.mean = TRUE,
  unit = "",
  label_mean = "ALL:",
  ...
)
```

## Arguments

- x:

  rast or array

- y:

  rast or array to mean (x is used only for the range in this case)

- text.width:

  Longitude in decimals

- dig:

  vector with number of digits for plot

- xjust:

  passed to legend

- yjust:

  passed to legend

- horiz:

  passed to legend

- y.intersp:

  passed to legend

- x.intersp:

  passed to legend

- show.mean:

  set TRUE to hide mean value

- unit:

  a string for units

- label_mean:

  label in case y is provided

- ...:

  extra arguments passed to legend

## Value

No return value

## Note

for use with rast use before any change of projection

text.width can vary depending on map dimensions

## Examples

``` r
x <- 1:10 + rnorm(10,sd = .4)
plot(x,ty='l')
legend_range(x)

```
