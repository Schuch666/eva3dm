#' Plot or add points using a color scale
#'
#' @description Custon plot for SpatRaster (terra R-package) object based on terra package
#'
#' @param p SpatVector points
#' @param z column name or a vector of values to plot
#' @param col color for the point
#' @param col2 color for the outline
#' @param lim range of values for scale
#' @param symmetry calculate symmetrical scale
#' @param pch type of point
#' @param pch2 type of point for contour
#' @param cex character expansion for the points
#' @param cex2 character expansion for the contour
#' @param outside to include values outside range
#' @param add add to existing plot
#' @param plg list of parameters passed to terra::add_legend
#' @param pax list of parameters passed to graphics::axis
#' @param unit used title in terra::add_legend
#' @param expand to expand the plot region
#' @param ... arguments to be passing to terra::plot
#'
#' @return No return value
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
overlay <- function(p,z,col,col2,
                    lim      = range(z, na.rm = TRUE),
                    symmetry = TRUE,
                    pch      = 19,
                    pch2     = NA,
                    cex      = 1.0,
                    cex2     = 1.2 * cex,
                    outside  = TRUE,
                    add      = FALSE,
                    plg      = list(tic = 'none',shrink=1.00),
                    pax      = list(),
                    unit,
                    expand   = 1.15,
                    ...){

  if(is.na(pch2) && pch == 19)
    pch2 = 1                      # nocov

  if(!missing(unit))
    plg = c(plg,list(title = unit)) # nocov

  if(missing(p))
    stop('p is missing') # nocov
  if(missing(z))
    z <- 'NMB (%)'       # nocov
  if(is.character(z)){
    z <- as.data.frame(p[,z])[,1] # nocov
  }
  if(missing(col2)){
    col2 = 'black'
  }

  if(missing(col))
    col <- c("#1B2C62","#204385","#265CA9","#4082C2",
             "#5DA9DB","#80C4EA","#A4DDF7","#C1E7F8",
             "#DEF2FA","#F2FAFD","#FFFFFF","#FFFFFF",
             "#FEFAE6","#FDF0B4","#FDDA7C","#FEBC48",
             "#FB992F","#F7762B","#E84E29","#D72828",
             "#B81B22","#97161A","#921519")

  if(symmetry){
    max  <- abs(max(lim, na.rm = TRUE))
    min  <- abs(min(lim, na.rm = TRUE))
    lim <- c(-max(max,min),max(max,min))
  }

  if(add == FALSE){
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

  if(length(col2) > 1){
    nlevels = length(col2)                                    # nocov
    levels <- seq(lim[1],lim[2],length.out = nlevels)         # nocov
    colz2  <- col2[cut(c(lim[1],z,lim[2]),nlevels,            # nocov
                       include.lowest = TRUE,labels = FALSE)] # nocov
    colz2  <- colz2[-1]                                       # nocov
    colz2  <- colz2[-length(colz2)]                           # nocov
  }else{
    colz2 = col2
  }

  ge <- terra::geom(p)
  points(x = ge[,'x'],
         y = ge[,'y'],
         col = colz2,
         pch = pch2,
         cex = cex2, ... )
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
#' @return SpatVector (terra package)
#'
#' @import terra
#'
#' @export
#'
#' @examples
#'
#' sites <- data.frame(lat = c(-22.72500,-23.64300,-20.34350),
#'                     lon = c(-47.34800,-46.49200,-40.31800),
#'                     row.names = c('Americana','SAndre','VVIbes'),
#'                     stringsAsFactors = F)
#' model<- readRDS(paste0(system.file("extdata",package="eva3dm"),"/model.Rds"))
#' obs  <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/obs.Rds"))
#'
#' # evaluation by station
#' stats <- eva(mo = model, ob = obs, site = "Americana")
#' stats <- eva(mo = model, ob = obs, site = "SAndre",table = stats)
#' stats <- eva(mo = model, ob = obs, site = "VVIbes",table = stats)
#' # evaluation using all stations
#' stats <- eva(mo = model, ob = obs, site = "ALL", table = stats)
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
