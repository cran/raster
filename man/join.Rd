\name{bind}

\docType{methods}

\alias{bind}
\alias{bind,SpatialPolygons,SpatialPolygons-method}
\alias{bind,SpatialLines,SpatialLines-method}
\alias{bind,SpatialPoints,SpatialPoints-method}

\title{ 
Bind Spatial* objects
}

\description{
Bind (append) Spatial* objects into a single object. All objects must be of the same vector type base class (SpatialPoints, SpatialLines, or SpatialPolygons)
}

\usage{
\S4method{bind}{SpatialPolygons,SpatialPolygons}(x, y, ..., keepnames=FALSE)

\S4method{bind}{SpatialLines,SpatialLines}(x, y, ..., keepnames=FALSE)

\S4method{bind}{SpatialPoints,SpatialPoints}(x, y, ..., keepnames=FALSE)
}


\arguments{
 \item{x}{Spatial* object}
 \item{y}{Spatial* object}
 \item{...}{Additional Spatial* objects}
 \item{keepnames}{Logical. If \code{TRUE} the row.names are kept (if unique)} 
}

\value{
Spatial* object
}

\examples{
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	mersch <- p[p$NAME_2=='Mersch', ]
	diekirch <- p[p$NAME_2=='Diekirch', ]
	remich <- p[p$NAME_2=='Remich', ]
	remich$NAME_1 <- NULL
	x <- bind(mersch, diekirch, remich)
	plot(x)
	data.frame(x)
}
}

\keyword{methods}
\keyword{spatial}
