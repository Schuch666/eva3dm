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

  sites[1,1] =  5.9
  sites[2,1] = -34
  sites[3,2] = -33
  sites[4,2] = -74.5

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

  # just the example
  dir.create(file.path(tempdir(), "MEAN"))
  folder <- system.file("extdata",package="eva3dm")
  wrf_file <- paste0(folder,"/wrf.day1.o3.nc")
  extract_mean(filelist = wrf_file,
               prefix = paste0(file.path(tempdir(),"MEAN"),'/mean'))

  # just the example
  dir.create(file.path(tempdir(), "MDA8"))
  folder <- system.file("extdata",package="eva3dm")
  wrf_file <- paste0(folder,"/test_small_o3.nc")
  extract_max_8h(filelist = wrf_file,
                 prefix = paste0(file.path(tempdir(),"MDA8"),'/mean'),
                 field = '3d')

  expect_equal(dim(ij),c(2,12))
})
