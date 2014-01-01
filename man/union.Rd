\name{union}

\docType{methods}

\alias{union}
\alias{union,Extent,Extent-method}
\alias{union,SpatialPolygons,SpatialPolygons-method}


\title{ 
Union Extent or SpatialPolygons* objects
}

\description{
Extent objects: Objects are combined into their union. See \code{\link{crop}} and \code{\link{extend}} to union a Raster object with an Extent object.

SpatialPolygons* objects. Overlapping polygons are intersected, other spatial objects are appended. Tabular attributes are joined.
}

\usage{
\S4method{union}{Extent,Extent}(x, y)

\S4method{union}{SpatialPolygons,SpatialPolygons}(x, y)

}

\arguments{
  \item{x}{Extent or SpatialPolygons* object}
  \item{y}{Same as \code{x}}
} 

\value{
Extent or SpatialPolygons object
}

\seealso{
\code{\link[raster]{intersect}, \link[raster]{extent}, \link[raster]{setExtent}}

\code{\link[sp]{merge}} for merging a data.frame with attributes of Spatial objects 
and \code{\link{+,SpatialPolygons,SpatialPolygons-method}} for an algebraic notation
}


\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
union(e1, e2)

#SpatialPolygons
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	p0 <- aggregate(p)
	b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
	projection(b) <- projection(p)
	u <- union(p0, b)
	plot(u, col=2:4)
}
}


\keyword{methods}
\keyword{spatial}

