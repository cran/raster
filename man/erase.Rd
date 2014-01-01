\name{erase}

\docType{methods}

\alias{erase}
\alias{erase,SpatialPolygons,SpatialPolygons-method}

\title{ 
Erase parts of a Spatial* object
}

\description{
Erase parts of a Spatial* objects with another Spatial* object
}

\usage{
\S4method{erase}{SpatialPolygons,SpatialPolygons}(x, y, ...)
}

\arguments{
  \item{x}{Spatial* object}
  \item{y}{Spatial* object}
  \item{...}{Additional arguments (none)}  
}

\value{
Spatial*
}

\author{Robert J. Hijmans}

\examples{
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
	projection(b) <- projection(p)
	e <- erase(p, b)
	plot(e)
}
}

\keyword{methods}
\keyword{spatial}

