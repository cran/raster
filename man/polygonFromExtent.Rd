\name{polygonFromExtent}
\alias{polygonFromExtent}


\title{Polygon from Extent}

\description{
polygonFromExtent creates a SpatialPolygon object from a Extent object (or a Raster*) object
}

\usage{
polygonFromExtent(bndbox, sp=TRUE)
}

\arguments{
  \item{bndbox}{An object of class Extent (which you can create with extent() )}  
  \item{sp}{logical. If \code{TRUE}, the output will be a SpatialPolygons object (sp package). Otherwise a matrix of coordinates is returned}    
}

\value{
a matrix or a SpatialPolygons object
}  
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{cellsFromExtent}}, \code{\link[raster]{extent}}, \code{\link[sp]{SpatialPolygons}} }

\examples{
r <- raster()
e <- extent(r)
pol <- polygonFromExtent(e)
}

\keyword{spatial}


