\name{brick}

\docType{methods}

\alias{brick}

\alias{brick,character-method}
\alias{brick,RasterLayer-method}
\alias{brick,RasterStack-method}
\alias{brick,RasterBrick-method}
\alias{brick,Extent-method}
\alias{brick,list-method}
\alias{brick,missing-method}
\alias{brick,SpatialPixels-method}
\alias{brick,SpatialGrid-method}

\title{ Create a RasterBrick object}

\description{
A RasterBrick is a multi-layer raster object. They are typically created from a multi-band file; but they can also exist entirely in memory. They are similar to a RasterStack, but processing time should be shorter when using a RasterBrick. Yet they are less flexible as they can only point to a single file.

A RasterBrick can be created from RasterLayer objects, from a RasterStack, or from a file. The can also be created from SpatialPixels and SpatialGrid objects.
}

\usage{brick(x, ...)}

\arguments{
 \item{x}{ Filename (character), Raster* object, SpatialGrid*, SpatialPixels*, or list of Raster* objects }
 \item{...}{ Additional Raster* objects }
}


\value{
RasterBrick object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}} }

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
b
nlayers(b)
layerNames(b)
extract(b, 870)
}

\keyword{methods}
\keyword{spatial}
