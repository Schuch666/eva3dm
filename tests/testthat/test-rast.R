test_that("rast based functions are ok", {
  library(terra)

  wrf    <- paste(system.file("extdata", package = "eva3dm"),"/wrfinput_d01", sep="")
  r      <- wrf_rast(file=wrf, name='XLAT')
  r_ncdf <- rast_to_netcdf(r)

  # copy of the original file to a temporary folder
  temp <- file.path(tempdir(),"test")
  dir.create(temp)
  copy <- paste0(temp,"/wrfinput_d01")
  file.copy(from = wrf,to = copy, overwrite = T)
  # save the original data in the copy
  rast_to_netcdf(r = r, file=copy, name='XLAT')

  plot_rast(r, range = c(-100,100), unit = 'test',grid = TRUE, add_range = TRUE)
  rr = -r

  plot_rast(rr, log = TRUE, proj = TRUE)
  plot_rast(rr, log = TRUE, proj = TRUE, max = 4)
  legend_range(r,rr)

  r2 = r
  r2[] = 2
  plot_rast(r2)

  p         <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))
  p$id      <- row.names(p)
  point     <- terra::vect(p)
  point$NMB <- 1:45 - 20 # some values to plot
  overlay(point,point$NMB,cex = 1.4, add = TRUE) #

  overlay(point,point$NMB,cex = 1.4, add = FALSE, main = 'new plot')

  plot_diff(r,r2)

  expect_equal(dim(r_ncdf), c(99,149,1))
})
