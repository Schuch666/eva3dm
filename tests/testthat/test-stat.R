test_that("stat works", {

  model <- 1:100
  data  <- model + rnorm(100,0.2)

  a <- stat(model = model, observation = data)

  expect_equal(a, stat(model = model, observation = data))

})
