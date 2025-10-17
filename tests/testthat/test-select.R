test_that("select works!", {

  model <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/model.Rds"))
  a     <- select(data = model, day  = 'weekday')
  b     <- select(data = model, day  = 'weekend')
  c     <- select(data = model, start = '2012-01-05', end  = '2012-01-09')
  d     <- select(data = model, year = 2012,month = 1, day = 1,hour = 0,minutes = 0,seconds = 0)
  e     <- select(data = model, month = 'JAN')
  f     <- select(data = model, day = 'tue')
  g     <- select(data = model, range = model)
  h     <- select(data = model, julian = 1)

  expect_equal(nrow(a) + nrow(b), nrow(model))
})
