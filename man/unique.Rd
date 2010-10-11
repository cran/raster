\name{unique}

\docType{methods}

\alias{unique}
\alias{unique,RasterLayer,missing-method}

\title{Unique values}

\description{
This function returns the unique values in a RasterLayer
}

\usage{
unique(x, incomparables=FALSE, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{incomparables}{Ignored. Must be missing}
  \item{...}{Ignored. Must be missing}
}

\seealso{ \code{\link[base]{unique}} }

\value{
a vector
}

\author{Robert J.Hijmans}

\examples{
r <- raster(ncol=10, nrow=10)
r[] <- round(runif(ncell(r))*10)
unique(r)
}

\keyword{spatial}
