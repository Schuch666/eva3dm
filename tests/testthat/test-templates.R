test_that("template works", {

  f <- file.path(tempdir(),"POST")

  template(root = f, template = 'WRF',scheduler = "PBS") # WRF post process
  template(root = f, template = 'WRF-3')                 # WRF post process for 3 domain
  template(root = f, template = 'CAMx')                  # CAMx post process
  template(root = f, template = 'WRF-Chem')              # WRF-Chem post process
  template(root = f, template = 'EXP')                   # WRF-Chem post process
  template(root = f, template = 'METAR')                 # download METAR
  template(root = f, template = 'MET')                   # eval met
  template(root = f, template = 'MET-3')                 # eval met for 3 domains
  template(root = f, template = 'AQ')                    # eval QA
  template(root = f, template = 'SAT')                   # eval sat rain
  template(root = f, template = 'PSA')                   # CDO sat pros processing
  template(root = f, template = 'AQS_BR')                # download aqs for SP and RJ
  template(root = f, template = 'INMET')                 # process conventional and automatic INMET stations
  template(root = f, template = 'merge')                 # template to merge all INMET observations and METAR observations
  template(root = f, template = 'ISD')                   # process ISD observations

  expect_equal(length(f), 1)
})
