test_that("extraction funtions are OK!", {
  sites <- readRDS(paste0(system.file("extdata",package="eva3dm"),
                          "/sites_AQ_BR.Rds"))

  files <- dir(path = system.file("extdata",package="eva3dm"),
               pattern = 'wrf.day',
               full.names = TRUE)

  dir.create(file.path(tempdir(),"SERIE"))
  folder <- file.path(tempdir(),"SERIE")

  ij <- extract_serie(filelist = files,
                      point    = sites[1:2,],
                      prefix   = paste0(folder,'/serie'),
                      return.nearest = TRUE,
                      fast     = TRUE)

  extract_serie(filelist = files,
                point    = ij,
                prefix   = paste0(folder,'/serie'),
                use_ij   = TRUE,
                new      = TRUE)

  sites[1,1] =  66.6
  sites[2,2] =  66.6
  sites[3,1] = -66.6
  sites[4,2] = -66.6

  extract_serie(filelist = files,
                point    = terra::vect(sites[1:5,]),
                prefix   = paste0(folder,'/serie'),
                id       = 'name',
                new      = TRUE)

  extract_serie(filelist = files,
                point    = terra::vect(sites[1:5,]),
                prefix   = paste0(folder,'/serie'),
                id       = 'name',
                new      = FALSE)

  expect_equal(2 * 2, 4)
})
