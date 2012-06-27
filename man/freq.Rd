\name{freq}

\docType{methods}

\alias{freq}
\alias{freq,RasterLayer-method}

\title{Frequency table}

\description{
Frequency table of the values of a RasterLayer.  
}

\usage{
\S4method{freq}{RasterLayer}(x, digits=0, useNA='ifany', progress='', ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{digits}{Non-negative integer for rounding the cell values. Argument is passed to \code{\link[base]{round}} }
  \item{useNA}{Character. What to do with NA values? Options are "no", "ifany", "always". See to \code{\link[base]{table}} }
  \item{progress}{Character to specify a progres bar. Choose from 'text', 'window', or '' (the default, no progress bar)}
  \item{...}{Additional arguments (none implemented)}
}

\value{
matrix
}

\seealso{ \code{\link[raster]{count}}, \code{\link[raster]{crosstab} } and \code{\link[raster]{zonal} } }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r))
r <- r * r * r * 10
freq(r)
}

\keyword{spatial}
\keyword{univar}
