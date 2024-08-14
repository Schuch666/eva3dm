test_that("sat works", {

  m <- terra::rast(paste0(system.file("extdata",package="eva3dm"),"/camx_no2.Rds"))
  o <- terra::rast(paste0(system.file("extdata",package="eva3dm"),"/omi_no2.Rds"))
  ex <- read_stat(paste0(system.file("extdata",package="eva3dm"),"/sat_cat.csv"),verbose = T)
  ex <- read_stat(paste0(system.file("extdata",package="eva3dm"),"/sat_cat.txt"),verbose = T)

  dir.create(file.path(tempdir(), "s"))
  write_stat(file    = paste0(file.path(tempdir(), "s"),'/sat_cat.csv'),
             stat    = ex,
             verbose = TRUE)
  write_stat(file    = paste0(file.path(tempdir(), "s"),'/sat_cat.txt'),
             stat    = ex,
             verbose = TRUE)

  exn <- sat(m %IN% o,o,rname = 'NO2_categorical',eval_function = cate,threshold = 3.0, verbose = T)

  o2   <- o; add(o2) <- o
  m2   <- m; add(m2) <- m
  exn2 <- sat(m2,o2,eval_function = cate,threshold = 3.0,min = 2,max = 110)

  model <- 0.02 * 1:100
  set.seed(666)
  data  <- abs(rnorm(100,0.01))

  cate(model = model, observation = data, threshold = 1,
       to.plot = TRUE, rname = 'example',cutoff = c(0.05,3))

  cate(model = model, observation = data, threshold = 1,
       to.plot = TRUE)

  expect_equal(exn, ex)
})
