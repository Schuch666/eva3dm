#' Plot or add points using a color scale
#'
#' @description Custon plot for SpatRaster (terra R-package) object based on terra package
#'
#' @param p SpatVector points
#' @param z column name or a vector of values to plot
#' @param col color
#' @param lim range of values for scale
#' @param symmetry calculate symmetrical scale
#' @param pch type of point
#' @param cex character expansion for the points
#' @param outside to include values outside range
#' @param add add to existing plot
#' @param plg list of parameters passed to terra::add_legend
#' @param pax list of parameters passed to graphics::axis
#' @param expand to expand the plot region
#' @param ... arguments to be passing to terra::plot
#'
#' @import terra
#'
#' @export
#'
#' @examples
#' sp<- terra::vect(paste0(system.file("extdata",package="eva3dm"),"/masp.shp"))
#' BR<- terra::vect(paste0(system.file("extdata",package="eva3dm"),"/BR.shp"))
#'
#' p    <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))
#' p$id      <- row.names(p)
#' point     <- terra::vect(p)
#' point$NMB <- 1:45 - 20 # some values to plot
#'
#' terra::plot(BR, main = 'add points',xlim = c(-52,-37),ylim = c(-25,-18))
#' terra::lines(BR)
#' terra::lines(sp, col = 'gray')
#' overlay(point,point$NMB,cex = 1.4, add = TRUE)
#'
#' overlay(point,point$NMB,cex = 1.4, add = FALSE, main = 'new plot')
#' terra::lines(BR)
#' terra::lines(sp, col = 'gray')
#'
overlay <- function(p,z,col,
                    lim      = range(z, na.rm = TRUE),
                    symmetry = TRUE,
                    pch      = 19,
                    cex      = 1.0,
                    outside  = TRUE,
                    add      = FALSE,
                    plg      = list(tic = 'none',shrink=1.00),
                    pax      = list(),
                    expand   = 1.15,
                    ...){

  if(missing(p))
    stop('p is missing') # nocov
  if(missing(z))
    z <- 'NMB (%)'       # nocov
  if(is.character(z)){
    z <- as.data.frame(p[,z])[,1] # nocov
  }

  if(missing(col))
    col <- c("#1B2C62","#204385","#265CA9","#4082C2",
             "#5DA9DB","#80C4EA","#A4DDF7","#C1E7F8",
             "#DEF2FA","#F2FAFD","#FFFFFF","#FFFFFF",
             "#FEFAE6","#FDF0B4","#FDDA7C","#FEBC48",
             "#FB992F","#F7762B","#E84E29","#D72828",
             "#B81B22","#97161A","#921519")

  if(symmetry){
    max  <- abs(max(lim, na.rm = T))
    min  <- abs(min(lim, na.rm = T))
    lim <- c(-max(max,min),max(max,min))
  }

  if(add == F){
    r <- rast(x = expand * terra::ext(p))
    values(r) = 666
    terra::plot(r,col = col,range = lim,
                legend =TRUE,
                axes =TRUE,
                type = "continuous",
                plg = plg, pax = pax, ...)
  }
  nlevels = length(col)
  if(outside){
    z[z >= lim[2]] = lim[2]
    z[z <= lim[1]] = lim[1]
  }
  levels <- seq(lim[1],lim[2],length.out = nlevels)
  colz   <- col[cut(c(lim[1],z,lim[2]),nlevels,
                    include.lowest = TRUE,labels = FALSE)]
  colz   <- colz[-1]
  colz   <- colz[-length(colz)]

  ge <- terra::geom(p)
  points(x = ge[,'x'],
         y = ge[,'y'],
         col = colz,
         pch = pch,
         cex = cex, ... )
}

#' Combine stats and site list to overlay plot
#' @description combines the stats (from individual station evaluation) and site list in a SpatVector using row.names
#'
#' @param stat data.frame with stats or other variable (containing row.names and other variables)
#' @param site data.frame with site list (containing row.names, lat and lon)
#'
#' @export
#' @import terra
#'
#' @examples
#'
#' sites<- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))
#' model<- readRDS(paste0(system.file("extdata",package="eva3dm"),"/model.Rds"))
#' obs  <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/obs.Rds"))
#'
#' stats <- eva(mo = model, ob = obs, site = 'Americana')
#' stats <- eva(mo = model, ob = obs, site = 'SAndre',table = stats)
#' stats <- eva(mo = model, ob = obs, site = 'VVIbes',table = stats)
#'
#' print(stats)
#'
#' geo_stats <- stats %at% sites
#'
#' print(geo_stats)
#'
`%at%` <- function(stat, site){

  cat('georeferencing',deparse(substitute(stat)),'at',deparse(substitute(site)),'\n')

  stat$site    <- row.names(stat)
  site$site    <- row.names(site)
  a            <- merge(site, stat, by = 'site')
  row.names(a) <- a$site
  sites        <- terra::vect(a)
  return(sites)
}
