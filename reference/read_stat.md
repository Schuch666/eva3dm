# Function to read stats and evaluation

Function to read stats and evaluation output

## Usage

``` r
read_stat(file, sep = ";", dec = ".", verbose = FALSE, ...)
```

## Arguments

- file:

  model data.frame

- sep:

  the field separator string, passed to read.table function

- dec:

  he string to use for decimal points, passed to read.table function

- verbose:

  display additional information

- ...:

  arguments passed to read.table functions

## Value

No return value

## Examples

``` r
sample <- read_stat(file    = paste0(system.file("extdata", package = "eva3dm"),"/sample.txt"),
                    verbose = TRUE)
#> reading /home/runner/work/_temp/Library/eva3dm/extdata/sample.txt 

sample <- read_stat(file    = paste0(system.file("extdata", package = "eva3dm"),"/sample.csv"),
                    verbose = TRUE)
#> reading /home/runner/work/_temp/Library/eva3dm/extdata/sample.csv 
```
