\name{intersectExtent}

\alias{intersectExtent}
  
\title{Extent intersection}

\description{
Obsolete. See \code{\link{intersect}}
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

\seealso{ \code{\link[raster]{union}, \link[raster]{intersect}, \link[raster]{extent}} }

\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
intersectExtent(e1, e2)
}

\keyword{spatial}
