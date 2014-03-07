\name{intersect}

\docType{methods}

\alias{intersect}
\alias{intersect,Extent,ANY-method}
\alias{intersect,Raster,ANY-method}
\alias{intersect,SpatialPolygons,SpatialPolygons-method}
\alias{intersect,SpatialPoints,ANY-method}

\title{ 
Intersect
}

\description{
Extent objects: Returns the intersection, i.e. the area of overlap of two Extent objects. The second argument can also be any argument from which an Extent object can be extracted. 

If the first object is a Raster* object: this function is equivalent to \code{\link{crop}}. 

SpatialPolygons* objects: Only the overlapping areas (if any) are returned. For SpatialPolygonDataFrame objects, the data.frames are also merged. 

SpatialPoints* objects: Only the points that overlap with the extent of object \code{y} are returned.  
}

\usage{
\S4method{intersect}{Extent,ANY}(x, y)

\S4method{intersect}{Raster,ANY}(x, y)

\S4method{intersect}{SpatialPolygons,SpatialPolygons}(x, y)

\S4method{intersect}{SpatialPoints,ANY}(x, y)
}

\arguments{
  \item{x}{Extent, Raster*, SpatialPolygons* or SpatialPoints* object}
  \item{y}{same as \code{x}. Except when \code{x} is a Raster* or SpatialPoints* object, this should be an Extent object, or any object from which an Extent can be extracted}
} 

\value{
Extent, Raster*, or SpatialPolygons object
}

\seealso{
\code{\link{union}, \link[raster]{extent}, \link{crop}}
}

\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
intersect(e1, e2)

#SpatialPolygons
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
	projection(b) <- projection(p)
	i <- intersect(p, b)
	plot(p)
	plot(b, add=TRUE, col='red')
	plot(i, add=TRUE, col='blue', lwd=2)
}
}


\keyword{methods}
\keyword{spatial}

