library(eva3dm)
library(terra)

# read XLAT from a NetCDF
wrf <- paste(system.file("extdata", package = "eva3dm"), "/wrfinput_d01", sep="")
r   <- wrf_rast(file=wrf, name='XLAT')
# copy the object and re=place with random values
r2  <- r; r2[] = - 23 + 3 * runif(ncell(r))

# some fake vcalues of mean bias (MB) for each air quality station on Brazil
p         <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))
p$id      <- row.names(p)
point     <- terra::vect(p)
point$MB  <- 1:45
# load a map for the overlay
map <- terra::vect(paste0(system.file("extdata",package="eva3dm"),"/BR.shp"))

png(filename = 'joss_1.png',width = 1600,height = 1000,pointsize = 22)

oldpar <- par(mfrow = c(2,2))

plot_rast(r, main = 'plot_rast() example, WRF XLAT variable',grid = TRUE,grid_col = 'white',add_range = TRUE,color = 'eva4',unit = 'degree\nnorth')

overlay(point,'MB',cex = 1.4,main = 'overlay(), example for Mean Bias (MB)',xlim = c(-52.1,-37),ylim = c(-26.1,-17.9),plg = list(title = '%'))
terra::lines(map)
legend_range(point$MB)

plot_diff(x = r2, y = r, main = 'plot_diff() example, relative difference',grid = TRUE,grid_col = 'black',absolute = FALSE,add_range = TRUE)
plot_diff(x = r2, y = r, main = 'plot_diff() example, absolute difference',grid = TRUE,grid_col = 'black',relative = FALSE,unit = 'degree\nnorth',add_range = TRUE)
dev.off()

par(oldpar)
