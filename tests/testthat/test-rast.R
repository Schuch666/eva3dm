test_that("rast based functions are ok", {
  library(terra)

  wrf    <- paste(system.file("extdata", package = "eva3dm"),"/wrfinput_d01", sep="")
  r      <- wrf_rast(file=wrf, name='XLAT')
  r_ncdf <- rast_to_netcdf(r)

  # copy of the original file to a temporary folder
  temp <- file.path(tempdir(),"test_rast")
  dir.create(temp)
  copy <- paste0(temp,"/wrfinput_d01")
  file.copy(from = wrf,to = copy, overwrite = T)
  # save the original data in the copy
  rast_to_netcdf(r = r, file=copy, name='XLAT')

  plot_rast(r, range = c(-100,100), unit = 'test',grid = TRUE, add_range = TRUE,scale = 1)
  rr = -r

  plot_rast(rr, log = TRUE, proj = TRUE)
  plot_rast(rr, log = TRUE, proj = TRUE, max = 4)
  legend_range(r,rr)

  r2 = r
  r2[] = 2
  r2[50,100] = NA
  plot_rast(r2, fill = TRUE)

  p         <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))
  p$id      <- row.names(p)
  point     <- terra::vect(p)
  point$NMB <- 1:45 - 20 # some values to plot
  overlay(point,point$NMB,cex = 1.4, add = TRUE)

  overlay(point,point$NMB,cex = 1.4, add = FALSE, main = 'new plot')

  plot_diff(r,r2)

  O34d <- wrf_sds(paste0(system.file("extdata",package="eva3dm"),"/wrf_4d_o3_Boston.nc"),'o3')
  O3_column <- calculate_column(paste0(system.file("extdata",package="eva3dm"),"/wrf_column_o3_Boston.nc"),'o3')

  expect_equal(dim(r_ncdf), c(149,99,1))
})
