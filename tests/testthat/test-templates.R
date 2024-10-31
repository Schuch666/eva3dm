test_that("template works", {

  f <- file.path(tempdir(),"POST")

  template(root = f, template = 'WRF',scheduler = "PBS") # WRF post process
  template(root = f, template = 'WRF-Chem')              # WRF-Chem post process
  template(root = f, template = 'IPEN')                  # WRF-Chem post process
  template(root = f, template = 'METAR')                 # download METAR
  # template(root = f, template = 'AQ')                  # eval QA
  # template(root = f, template = 'MET')                 # eval met
  # template(root = f, template = 'GPCP')                # eval sat rain

  expect_equal(length(f), 1)
})
