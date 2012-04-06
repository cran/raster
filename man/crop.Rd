\name{crop}

\alias{crop}
\alias{crop,Raster-method}
\alias{crop,Spatial-method}

\title{Crop}

\description{
crop returns a geographic subset of an object as specified by an Extent object (or object from which an extent object can be extracted/created). 
If \code{x} is a Raster* object, the Extent is aligned to \code{x}. Areas included in \code{y} but outside the extent of \code{x} are ignored (see \code{\link{expand}} if you want a larger area)
}

\usage{
\S4method{crop}{Raster}(x, y, filename="", snap='near', ...)

\S4method{crop}{Spatial}(x, y, ...)
}

\arguments{
  \item{x}{Raster* object or SpatialPolygons* or SpatialLines* object}
  \item{y}{Extent object, or any object from which an Extent object can be extracted (see Details)}
  \item{filename}{Character, output filename. Optional}
  \item{snap}{Character. One of 'near', 'in', or 'out', for use with \code{\link{alignExtent}}}  
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}

\details{
Objects from which an Extent can be extracted/created include RasterLayer, RasterStack, RasterBrick and objects of the Spatial* classes from the sp package. You can check this with the \code{\link[raster]{extent}} function. New Extent objects can be also be created with function \code{\link{extent}} and \code{\link{drawExtent}} by clicking twice on a plot.
}

\value{
RasterLayer or RasterBrick object; or SpatialLines or SpatialPolygons object.
}

\author{Robert J. Hijmans}

\seealso{ 
\code{\link[raster]{expand}}, \code{\link[raster]{merge}}
}

\examples{
r <- raster(nrow=45, ncol=90)
r[] <- 1:ncell(r)
e <- extent(-160, 10, 30, 60)
rc <- crop(r, e)	
}

\keyword{spatial}
