\name{count}

\alias{count}

\title{Count}

\description{
Count the frequency of a single value in a RasterLayer.  
}

\usage{
count(raster, value, digits=0, progress)
}

\arguments{
  \item{raster}{A RasterLayer}
  \item{value}{The value to be counted}  
  \item{digits}{the number of digits for rounding the values}   
  \item{progress}{Character. Choose from "text", "window", or "" (the default, no progress bar)}
}

\value{
numeric
}

\seealso{ \code{\link{freq}} }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r))
r <- r * r * r * 10
count(r, 5)
}

\keyword{univar}
