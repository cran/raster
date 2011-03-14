\name{atan2}


\alias{atan2,RasterLayer,RasterLayer-method}
\alias{atan2}

\title{Two argument arctangent function}

\description{
For RasterLayer arguments x and y, atan2(y, x) returns the angle in radians for the tangent y/x, handling the case when x is zero. See \code{link[base]{Trig}}

See \code{\link[raster]{Math-methods}} for other trigonometric and mathematical functions that can be used with a Raster* objects.
}

\usage{
atan2(y, x) 
}

\arguments{
  \item{y}{A RasterLayer object}
  \item{x}{A RasterLayer object}
}

\seealso{ \code{\link[raster]{Math-methods}} }

\author{Robert J. Hijmans}

\examples{
r1 <- r2 <- raster(nrow=10, ncol=10)
r1[] <- (runif(ncell(r1))-0.5) * 10
r2[] <- (runif(ncell(r1))-0.5) * 10
atan2(r1, r2)
}

\keyword{methods}
\keyword{math}
