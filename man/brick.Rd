\name{brick}

\docType{methods}

\alias{brick}

\alias{brick,character-method}
\alias{brick,Raster-method}
\alias{brick,RasterStack-method}
\alias{brick,RasterBrick-method}
\alias{brick,Extent-method}
\alias{brick,list-method}
\alias{brick,missing-method}
\alias{brick,SpatialPixels-method}
\alias{brick,SpatialGrid-method}

\title{ Create a Rasterbrick object}

\description{
A RasterBrick is a multi-layered RasterLayer. They are typically created from a multi-band file; but they can also exist entirely in memory. They are similar to a RasterStack, but processing time should be shorter. Yet they are less flexible as they can only point to a single file.

A RasterBrick can be created from RasterLayer objects, or from a file. The can also be created from SpatialPixels or SpatialGrid objects.
}

\section{Methods}{
\describe{
\code{brick(x, ...)}

  \item{\code{x}}{ filename (character), Raster* object, SpatialGrid*, SpatialPixels*, or list (of filenames and/or Raster* objects) }
  \item{\code{...}}{additional RasterLayer objects }

\code{x} can be a filename from which a RasterLayer can be created (with \code{raster}, a Raster* object, 
or a list of filenames and/or Raster* objects.

If \code{x} is a \code{list}, no additional arguments  \code{...} are accepted.

}}

\value{
  a RasterBrick object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}} }

\examples{
  b <- brick(system.file("pictures/Rlogo.jpg", package="rgdal"))
  b
  nlayers(b)
  layerNames(b)
  cellValues(b, 870)
}

\keyword{methods}
\keyword{spatial}
