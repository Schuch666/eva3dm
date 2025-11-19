# Create templates for model evaluation

Create templates of code (r-scripts and bash job-submission script) to
read, post-process and evaluate model results.

## Usage

``` r
template(
  root,
  template = "WRF",
  case = "case",
  env = "rspatial",
  scheduler = "SBATCH",
  partition = "main",
  project = "PROJECT",
  verbose = TRUE
)
```

## Arguments

- root:

  directory to create the template

- template:

  Character; One of of the following:

  |                                  |                      |                                          |                                    |                                      |
  |----------------------------------|----------------------|------------------------------------------|------------------------------------|--------------------------------------|
  | **argument**                     | **TYPE**             | **MODEL/OBS**                            | **OBSERVATION**                    | WRF                                  |
  | post-process                     | WRF                  | METAR and INMET                          | WRF-3                              | post-process                         |
  | WRF triple nested                | METAR                | WRF-Chem                                 | post-process                       | WRF-Chem                             |
  | METAR, AQS in Brazil and AERONET | EXP                  | post-process                             | WRF-Chem                           | METAR, PBL variables and composition |
  | CAMX                             | post-process         | CAMx                                     | AERONET                            | CAMX-3                               |
  | post-process                     | CAMx triple nested   | AERONET                                  | NCO                                | post-process                         |
  | WRF                              | Satellite            | terra                                    | post-process                       | WRF                                  |
  | Satellite                        | SAT                  | evaluation                               | WRF                                | Satellite (GPCP)                     |
  | MET                              | evaluation           | WRF                                      | METAR                              | MET-3                                |
  | evaluation                       | WRF triple nested    | METAR                                    | AQ                                 | evaluation                           |
  | WRF or CAMx                      | O3, Max O3 and PM2.5 | AQS_BR                                   | download                           | observations                         |
  | AQS in Brazil for SP and RJ      | METAR                | downlaad                                 | observations                       | METAR from ASOS                      |
  | INMET                            | pre-processing       | observations                             | INMET (automatic and conventional) | merge                                |
  | pre-processing                   | observations         | merge INMET (automatic and conventional) | ISD                                | pre-processing                       |

- case:

  case to be evaluated

- env:

  name of the conda environment

- scheduler:

  job scheduler used (SBATCH or PBS)

- partition:

  partition name

- project:

  project name

- verbose:

  display additional information

## Value

no value returned, create folders and other template scripts

## Examples

``` r
temp <- file.path(tempdir(),"POST")
template(root = temp,template = 'WRF', case = 'WRF-only')
#>  folder  /tmp/RtmpWjQDAv/POST/WRF/WRF-only : link wrf output files here!
#>  bash  /tmp/RtmpWjQDAv/POST/post-R_wrf.sh : post processing job script
#>  r-script /tmp/RtmpWjQDAv/POST/extract_metar.R : source code to extract metar using eva3dm::extract_serie()
#>  r-script /tmp/RtmpWjQDAv/POST/extract_inmet.R : source code to extract inmet using eva3dm::extract_serie()
```
