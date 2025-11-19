# Functions to write stats and evaluation

Functions to write the output from evaluation functions. If the file
name ends with .csv the function write.csv is used otherwise the
function write.table is used.

## Usage

``` r
write_stat(stat, file, sep = ";", dec = ".", verbose = FALSE, ...)
```

## Arguments

- stat:

  observed data.frame

- file:

  model data.frame

- sep:

  the field separator string, passed to write.table function

- dec:

  he string to use for decimal points, passed to write.table function

- verbose:

  display additional information

- ...:

  arguments passed to write.table and write.csv functions

## Value

No return value

## Examples

``` r
sample <- read_stat(paste0(system.file("extdata", package = "eva3dm"),"/sample.csv"),
                    verbose = TRUE)
#> reading /home/runner/work/_temp/Library/eva3dm/extdata/sample.csv 
dir.create(file.path(tempdir(), "stats"))

write_stat(file    = paste0(file.path(tempdir(), "stats"),'/sample.txt'),
           stat    = sample,
           verbose = TRUE)
#> writing /tmp/RtmpWjQDAv/stats/sample.txt 

write_stat(file    = paste0(file.path(tempdir(), "stats"),'/sample.csv'),
           stat    = sample,
           verbose = TRUE)
#> writing /tmp/RtmpWjQDAv/stats/sample.csv 
```
