test_that("eva works!", {

  model <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/model.Rds"))
  obs   <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/obs.Rds"))

  model$zero <- 0
  obs$zero   <- 1:length(model$zero)

  table <- eva(mo = model,
               ob = obs,
               site = "VVIbes",
               no_tz = TRUE)
  table <- eva(mo = model,
               ob = obs,
               site = "Ibirapuera",
               table = table)

  obs$Americana = obs$Americana + 400
  table <- eva(mo = model,
               ob = obs,
               site = "Americana",
               table = table,
               fair = names(model),
               wd = TRUE,
               cutoff = c(0,999),
               cutoff_NME = c(0,935),
               rname = 'am 2')

  table <- eva(mo = model,
               ob = obs,
               site = "Terradonunca",
               table = table)
  table <- eva(mo = model[,-2],
               ob = obs,
               site = "Americana",
               table = table)
  model$zeros <- 0
  obs$zeros   <- 1:length(obs$zeros)
  table <- eva(mo = model,
               ob = obs,
               site = "zeros",
               table = table,
               rname = 'model zero')
  obs$zeros   <- 0
  model$zeros <- 1:length(model$zeros)
  table <- eva(mo = model,
               ob = obs,
               site = "zeros",
               table = table,
               rname = 'obs zero')
  obs$zeros   <- 3
  model$zeros <- 1:length(model$zeros)
  table <- eva(mo = model,
               ob = obs,
               site = "zeros",
               table = table,
               rname = 'obs cke')
  table <- eva(mo = model,
               ob = obs,
               site = "ALL",
               table = table,
               fair = model,
               rname = 'all fair ')
  table <- eva(mo = model,
               ob = obs,
               site = "ALL",
               table = table,
               fair = names(model),
               rname = 'all fair 2')
  table <- eva(mo = model %IN% obs,
               ob = obs,
               site = "ALL",
               table = table,
               rname = 'all model in obs')
  table <- eva(mo = model %IN% names(obs),
               ob = obs,
               site = "ALL",
               table = table,
               rname = 'all names model in obs')
  print(table)

  a <- stat(1:1000 * 0 + 200, mod = 1:1000, wd = T, rname = 'nome')

  expect_equal(dim(table), c(12,11))
})
