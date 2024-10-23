test_that("template works", {

  folder = dir.create(file.path(tempdir(),"POST"))
  template(root = folder)

  expect_equal(length(folder), 1)
})
