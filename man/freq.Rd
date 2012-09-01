\name{freq}

\docType{methods}

\alias{freq}
\alias{freq,RasterLayer-method}
\alias{freq,RasterStackBrick-method}

\title{Frequency table}

\description{
Frequency table of the values of a RasterLayer.  
}

\usage{
\S4method{freq}{RasterLayer}(x, digits=0, useNA='ifany', progress='', ...)

\S4method{freq}{RasterStackBrick}(x, digits=0, useNA='ifany', merge=FALSE, progress='', ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{digits}{non-negative integer for rounding the cell values. Argument is passed to \code{\link[base]{round}} }
  \item{useNA}{character. What to do with NA values? Options are "no", "ifany", "always". See to \code{\link[base]{table}} }
  \item{progress}{character to specify a progress bar. Choose from 'text', 'window', or '' (the default, no progress bar)}
  \item{merge}{logical. If \code{TRUE} the list will be merged in a single data.frame}
  \item{...}{additional arguments (none implemented)}
}

\value{
matrix (RasterLayer). List of matrices (one for each layer) or data.frame (if \code{merge=TRUE}) (RasterStack or RasterBrick)
}

\seealso{ \code{\link[raster]{count}}, \code{\link[raster]{crosstab} } and \code{\link[raster]{zonal} } }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r))
r[1:5] <- NA
r <- r * r * r * 5
freq(r)

s <- stack(r, r*2, r*3)
freq(s, merge=TRUE)
}

\keyword{spatial}
\keyword{univar}
