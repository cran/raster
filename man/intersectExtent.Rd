\name{intersectExtent}

\alias{intersectExtent}
  
\title{Extent intersection}

\description{
intersectExtent returns the intersection (i.e. the area in which they all overlap) of multiple Extent objects
}

\usage{
intersectExtent(x, ..., validate=TRUE)
}

\arguments{
  \item{x}{Extent object or object from which it can be coerced via \code{\link[raster]{extent}} (Raster* or Spatial* objects)} 
  \item{...}{Additional Extent or Raster* or Spatial* objects } 
  \item{validate}{Logical. If \code{TRUE}, an error is returned if the intersection is empty; else \code{NULL} is returned if the intersection is empty}   
}
 
\value{
Extent object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{unionExtent}}, \code{\link[raster]{extent}}}

\examples{
r <- raster()
b1 <- extent(-10, 10, -20, 20)
b2 <- extent(0, 20, -40, 5)
i <- intersectExtent(b1, b2)
}

\keyword{spatial}
