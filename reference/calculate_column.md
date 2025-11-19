# Calculate column concentration of trace gases form WRF-Chem

Read output from WRF model and calculate the column of trace gases. The
column concentration \\ C\_{\mathrm{column}}\\ is computed as:

\$\$ C\_{\mathrm{column}} = \frac{N\_{A}}{R}
\int\_{\text{surface}}^{\text{model\\top}} C \frac{P}{T} \\ dz \$\$

where \\C\\ is the pollutant concentration, \\N\_{A}\\ is Avogadro's
number, \\R\\ is the universal gas constant, \\P\\ is the pressure
\[Pa\], \\T\\ is the temperature \[K\], and \\dz\\ is the layer
thickness \[m\].

## Usage

``` r
calculate_column(
  file = file.choose(),
  name,
  met,
  DU,
  flip_v = FALSE,
  flip_h = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- file:

  WRF output file, see notes

- name:

  trace gas name to be integrated

- met:

  (optional) WRF output file for meteorological variables, see notes

- DU:

  true to change the output units from 'molecules cm-1' to 'DU'

- flip_v:

  passed to wrf_rast, see notes

- flip_h:

  passed to wrf_rast, see notes

- verbose:

  display additional information

- ...:

  extra arguments passed to eva3dm::wrf_rast or eva3dm::wrf_sds

## Value

SpatRaster object (from terra package).

## Note

files in file should contain Times, XLAT, XLONG variables in addition to
the concentration to be integrated, the variable should include at least
3 dimensions including vertical.

met is a optional file, it should have containing PHB, PH, PB, P, and T
variables with the same dimension of the concentration integrated.

post processing can affect the orientation of the variables, the
arguments flip_v and flip_h and other arguments from eva3dm::wrf_rast
and eva3dm::wrf_sds can be used to take effect into account.

## Examples

``` r
file <- paste0(system.file("extdata",package="eva3dm"),"/wrf_column_o3_Boston.nc")
O3_column <- calculate_column(file,'o3', verbose = TRUE)
#> output unit set to DU for o3
#> reading meteorology from /home/runner/work/_temp/Library/eva3dm/extdata/wrf_column_o3_Boston.nc
#> reading o3 from /home/runner/work/_temp/Library/eva3dm/extdata/wrf_column_o3_Boston.nc
#> creating SpatRaster for o3 
```
