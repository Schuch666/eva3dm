test_that("rast based functions are ok", {
  library(terra)

  wrf    <- paste(system.file("extdata", package = "eva3dm"),"/wrfinput_d01", sep="")
  r      <- wrf_rast(file=wrf, name='XLAT')
  r_ncdf <- rast_to_netcdf(r)

  plot_rast(r, range = c(-100,100))
  rr = -r
  plot_rast(rr, log = TRUE, proj = TRUE)
  plot_rast(rr, log = TRUE, proj = TRUE, max = 4)

  expect_equal(dim(r_ncdf), c(99,149,1))
})
