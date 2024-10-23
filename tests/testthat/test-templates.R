test_that("template works", {

  f <- file.path(tempdir(),"POST")
  template(root = f)

  expect_equal(length(f), 1)
})
