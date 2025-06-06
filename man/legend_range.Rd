% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_legend.R
\name{legend_range}
\alias{legend_range}
\title{Plot a legend with the range of values}
\usage{
legend_range(
  x,
  y,
  text.width = NULL,
  dig = c(2, 2, 2),
  xjust = 0.005,
  yjust = 0.95,
  horiz = TRUE,
  y.intersp = 0.5,
  x.intersp = 0.5,
  show.mean = TRUE,
  unit = "",
  label_mean = "ALL:",
  ...
)
}
\arguments{
\item{x}{rast or array}

\item{y}{rast or array to mean (x is used only for the range in this case)}

\item{text.width}{Longitude in decimals}

\item{dig}{vector with number of digits for plot}

\item{xjust}{passed to legend}

\item{yjust}{passed to legend}

\item{horiz}{passed to legend}

\item{y.intersp}{passed to legend}

\item{x.intersp}{passed to legend}

\item{show.mean}{set TRUE to hide mean value}

\item{unit}{a string for units}

\item{label_mean}{label in case y is provided}

\item{...}{extra arguments passed to legend}
}
\value{
No return value
}
\description{
Plot a legend with the range of values
}
\note{
for use with rast use before any change of projection

text.width can vary depending on map dimensions
}
\examples{
x <- 1:10 + rnorm(10,sd = .4)
plot(x,ty='l')
legend_range(x)

}
