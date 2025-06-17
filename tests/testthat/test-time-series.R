test_that("mda8 / ma8h / hourly / daily are ok!", {

  model <- paste(system.file("extdata", package = "eva3dm"),
                 "/model_o3_ugm3_36km.Rds", sep="")
  model_data <- readRDS(model)

  model_mda8 <- mda8(model_data)
  model_8h   <- ma8h(model_data)

  model_houry <- hourly(model_data)
  model_daily <- daily(model_data)

  model_m <- monthly(model_daily)
  model_y <- yearly(model_daily)

  expect_equal(dim(model_daily)[1],
               dim(model_houry)[1]/24)
})
