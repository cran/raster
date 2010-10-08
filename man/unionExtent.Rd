\name{unionExtent}

\alias{unionExtent}
  
\title{Extent union}

\description{
unionExtent returns the union of multiple bounding boxes
}

\usage{
unionExtent(x, ...)
}

\arguments{
  \item{x}{A Extent or Raster* object } 
  \item{...}{ additional Extent or Raster* objects } 
}
 
\value{
a Extent object
}
  
\author{Robert J. Hijmans}

\seealso{  \code{\link[raster]{intersectExtent}}, \code{\link[raster]{extent}},  \code{\link[raster]{setExtent}}} 

\examples{
r <- raster()
b1 <- extent(-10, 10, -20, 20)
b2 <- extent(0, 20, -40, 5)
u <- unionExtent(b1, b2)
}

\keyword{spatial}
