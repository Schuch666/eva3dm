test_that("utility functios are functional!", {
  wrf    <- paste(system.file("extdata", package = "eva3dm"),"/wrfinput_d01", sep="")

  # copy of the original file to a temporary folder
  temp <- file.path(tempdir(),"test_utils")
  dir.create(temp)
  copy <- paste0(temp,"/wrfinput_d01")
  file.copy(from = wrf,to = copy, overwrite = T)

  ncdump(wrf)

  atr(wrf,0,verbose = T)
  atr(wrf,'Times',verbose = T)
  atr(wrf,'XLAT','units')
  atr(wrf,0,'DT', action = 'get')

  atr(copy,'XLAT','units',value = "degree north",verbose = T,action = 'write')
  atr(copy,0,'DT',value = 45,verbose = T,action = 'write')

  vars(wrf,action = 'print')
  v <- vars(wrf)

  expect_equal(length(v), 3)
})
