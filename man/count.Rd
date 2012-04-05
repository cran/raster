\name{count}

\alias{count,RasterLayer-method}
\alias{count,RasterStackBrick-method}
\alias{count}


\title{Count}

\description{
Count the frequency of a single value in a Raster object.  
}

\usage{
count(x, value, ...)
}

\arguments{
  \item{x}{A Raster* object}
  \item{value}{The value to be counted, can be a number or a logical or NA}  
  \item{...}{Additional arugments: \code{digits}, the number of digits for rounding the values, default is 0, i.e. numbers are compared as integers. }
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
