% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_overlay.R
\name{\%at\%}
\alias{\%at\%}
\title{Combine stats and site list to overlay plot}
\usage{
stat \%at\% site
}
\arguments{
\item{stat}{data.frame with stats or other variable (containing row.names and other variables)}

\item{site}{data.frame with site list (containing row.names, lat and lon)}
}
\value{
SpatVector (terra package)
}
\description{
combines the stats (from individual station evaluation) and site list in a SpatVector using row.names
}
\examples{

sites<- readRDS(paste0(system.file("extdata",package="eva3dm"),"/sites_AQ_BR.Rds"))
model<- readRDS(paste0(system.file("extdata",package="eva3dm"),"/model.Rds"))
obs  <- readRDS(paste0(system.file("extdata",package="eva3dm"),"/obs.Rds"))

stats <- eva(mo = model, ob = obs, site = 'Americana')
stats <- eva(mo = model, ob = obs, site = 'SAndre',table = stats)
stats <- eva(mo = model, ob = obs, site = 'VVIbes',table = stats)

print(stats)

geo_stats <- stats \%at\% sites

print(geo_stats)

}
