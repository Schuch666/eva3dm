# Plot the difference from two SpatRaster objects

Custom difference (x - y) plots for SpatRaster object (based on terra
package)

## Usage

``` r
plot_diff(
  x,
  y,
  col,
  absolute = FALSE,
  relative = FALSE,
  lim_a = NA,
  lim_r = NA,
  scale,
  unit = c(units(x), expression("%")),
  fill = FALSE,
  ...
)
```

## Arguments

- x:

  SpatVector points

- y:

  values to plot

- col:

  color

- absolute:

  to plot absolute difference

- relative:

  to plot relative difference

- lim_a:

  range of values for absolute scale

- lim_r:

  range of values for relative scale

- scale:

  variable multiplier for absolute difference

- unit:

  annotation for units

- fill:

  filling NAs

- ...:

  arguments to be passing to plot_raster

## Value

No return value

## Examples

``` r
folder <- system.file("extdata",package="eva3dm")
wrf    <- paste0(folder,"/wrfinput_d01")
A      <- wrf_rast(wrf,'XLAT')
terra::units(A) <- 'degrees'
B      <- wrf_rast(wrf,'XLONG')
plot_diff(A,B,int = 2)

```
