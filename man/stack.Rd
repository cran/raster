\name{stack}

\docType{methods}

\alias{stack}

\alias{stack,character-method}
\alias{stack,Raster-method}
\alias{stack,list-method}
\alias{stack,missing-method}
\alias{stack,SpatialPixelsDataFrame-method}
\alias{stack,SpatialGridDataFrame-method}

\title{ Create a RasterStack object}

\description{
A RasterStack is a collection of RasterLayer objects with the same spatial extent and resolution. 
A RasterStack can be created from RasterLayer objects, or from raster files, or both. The can also be created from SpatialPixels or SpatialGrid objects.
}

\section{Methods}{
\describe{
\code{stack(x, ...)}

  \item{\code{x}}{ filename (character), Raster* object, SpatialGrid*, SpatialPixels*, or list (of filenames and/or Raster* objects) }
  \item{\code{...}}{additional RasterLayer objects (or filenames) }
  \item{bands}{ optional argument. a vector or list of bands of raster data files}

\code{x} can be a filename from which a RasterLayer can be created (with \code{raster}, a Raster* object, 
or a list of filenames and/or Raster* objects.

The argument \code{bands} can be used to pass a vector of band numbers to indicated which band of a file (in a multiband file) should be used. 
A band numbers < 1 will lead to inclusion of all bands into the RasterStack. 

If \code{x} is a \code{list}, no additional arguments  \code{...} are accepted, except for the optional \code{bands} argument

}}

\value{
  a RasterStack object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{addLayer}}, \code{\link[raster]{dropLayer}}, \code{\link[raster]{raster}} }

\examples{
  fn <- system.file("external/test.grd", package="raster")
  s <- stack(fn, fn)
  r <- raster(fn)
  s <- stack(r, fn) 
}

\keyword{methods}
\keyword{spatial}
