# Function to return variable names

Return variable names of a NetCDF

## Usage

``` r
vars(file = NA, action = "get", verbose = FALSE)
```

## Arguments

- file:

  file name

- action:

  'get' to return variable names or 'print' to print

- verbose:

  display additional information

## Value

string

## Examples

``` r
vars(paste0(system.file("extdata",package="eva3dm"),'/wrfinput_d01'))
#> [1] "Times" "XLAT"  "XLONG"
```
