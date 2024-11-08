test_that("template works", {

  f <- file.path(tempdir(),"POST")

  template(root = f, template = 'WRF',scheduler = "PBS") # WRF post process
  template(root = f, template = 'WRF-3')                 # WRF post process for 3 domain
  # template(root = f, template = 'CAMx')                # CAMx post process (?)
  template(root = f, template = 'WRF-Chem')              # WRF-Chem post process
  template(root = f, template = 'EXP')                   # WRF-Chem post process
  template(root = f, template = 'METAR')                 # download METAR
  template(root = f, template = 'MET')                   # eval met
  template(root = f, template = 'MET-3')                 # eval met for 3 domains
  # template(root = f, template = 'AQ')                  # eval QA
  template(root = f, template = 'SAT')                   # eval sat rain

  expect_equal(length(f), 1)
})
