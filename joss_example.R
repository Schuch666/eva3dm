library(eva3dm)

# opening observation of 2m temperature from METAR
OBS       <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/metar.T2.Rds"))
# openeing data extracted from WRF-Chem model using extract_serie()
MODEL     <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/model.d03.T2.Rds"))
# converting from Kelving to Celcius
MODEL[-1] <- MODEL[-1] - 273.15
# perform the model evaluation
evaluation <- eva(mo = MODEL, ob = OBS, rname = 'T2 from WRF-Chem')
print(evaluation)
