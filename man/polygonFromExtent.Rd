\name{polygonFromExtent}
\alias{polygonFromExtent}


\title{Polygon from Extent}

\description{
polygonFromExtent creates a SpatialPolygon object from a Extent object (or a Raster*) object
}

\usage{
polygonFromExtent(x, sp=TRUE)
}

\arguments{
  \item{x}{An object of class Raster*, Extent, or Spatial*}  
  \item{sp}{logical. If \code{TRUE}, a SpatialPolygons object is returned. Otherwise a matrix of coordinates is returned}    
}

\value{
Matrix or a SpatialPolygons object
}  
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{cellsFromExtent}}, \code{\link[raster]{extent}}, \code{\link[sp]{SpatialPolygons}} }

\examples{
r <- raster()
e <- extent(r)
pol <- polygonFromExtent(e)
}

\keyword{spatial}


