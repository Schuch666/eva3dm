# Combine stats and site list to overlay plot

combines the stats (from individual station evaluation) and site list in
a SpatVector using row.names

## Usage

``` r
stat %at% site
```

## Arguments

- stat:

  data.frame with stats or other variable (containing row.names and
  other variables)

- site:

  data.frame with site list (containing row.names, lat and lon)

## Value

SpatVector (terra package)

## Examples

``` r
sites <- data.frame(lat = c(-22.72500,-23.64300,-20.34350),
                    lon = c(-47.34800,-46.49200,-40.31800),
                    row.names = c('Americana','SAndre','VVIbes'),
                    stringsAsFactors = F)
model<- readRDS(paste0(system.file("extdata",package="eva3dm"),"/model.Rds"))
obs  <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/obs.Rds"))

# evaluation by station
stats <- eva(mo = model, ob = obs, site = "Americana")
#> Americana has 227 valid observations
stats <- eva(mo = model, ob = obs, site = "SAndre",table = stats)
#> SAndre has 207 valid observations
stats <- eva(mo = model, ob = obs, site = "VVIbes",table = stats)
#> VVIbes has only 0 valid observations (lesser than 8 obs)
# evaluation using all stations
stats <- eva(mo = model, ob = obs, site = "ALL", table = stats)
#> combining all sites...
#> Americana(ok) SAndre(ok) VVIbes(skiped) ...
#>  total: 2 stations with valid data, 1 stations not included
#> ALL has 434 valid observations

print(stats)
#>             n      Obs      Sim         r       IOA       FA2     RMSE
#> Americana 227 40.92952 18.18197 0.6492022 0.5934335 0.4229075 32.47269
#> SAndre    207 60.62802 15.66744 0.6246762 0.4813952 0.1449275 59.21330
#> VVIbes      0       NA       NA        NA        NA        NA       NA
#> ALL       434 50.32488 16.98265 0.5573867 0.5085711 0.2903226 47.15775
#>                  MB       ME   NMB (%)  NME (%)
#> Americana -22.74754 24.10165 -55.57735 58.88575
#> SAndre    -44.96058 45.64932 -74.15808 75.29409
#> VVIbes           NA       NA        NA       NA
#> ALL       -33.34224 34.37900 -66.25398 68.31411

geo_stats <- stats %at% sites
#> georeferencing stats at sites 

print(geo_stats)
#>  class       : SpatVector 
#>  geometry    : points 
#>  dimensions  : 3, 12  (geometries, attributes)
#>  extent      : -47.348, -40.318, -23.643, -20.3435  (xmin, xmax, ymin, ymax)
#>  coord. ref. :  
#>  names       :      site     n   Obs   Sim      r    IOA    FA2  RMSE     MB
#>  type        :     <chr> <num> <num> <num>  <num>  <num>  <num> <num>  <num>
#>  values      : Americana   227 40.93 18.18 0.6492 0.5934 0.4229 32.47 -22.75
#>                   SAndre   207 60.63 15.67 0.6247 0.4814 0.1449 59.21 -44.96
#>                   VVIbes     0    NA    NA     NA     NA     NA    NA     NA
#>     ME NMB (%) NME (%)
#>  <num>   <num>   <num>
#>   24.1  -55.58   58.89
#>  45.65  -74.16   75.29
#>     NA      NA      NA
```
