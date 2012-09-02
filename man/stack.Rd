\name{stack}

\docType{methods}

\alias{stack}

\alias{stack,character-method}
\alias{stack,Raster-method}
\alias{stack,list-method}
\alias{stack,missing-method}
\alias{stack,SpatialPixelsDataFrame-method}
\alias{stack,SpatialGridDataFrame-method}
\alias{stack,kasc-method}


\title{Create a RasterStack object}

\description{
A RasterStack is a collection of RasterLayer objects with the same spatial extent and resolution. A RasterStack can be created from RasterLayer objects, or from raster files, or both. It can also be created from SpatialPixels or SpatialGrid objects.
}

\section{Methods}{
\describe{
\code{stack(x, ..., bands=NULL)}

\item{\code{x}}{ filename (character), Raster* object, SpatialGrid*, SpatialPixels*, or list (of filenames and/or Raster* objects). If \code{x} is a list, additional arguments  \code{...} are ignored.}
\item{\code{...}}{additional Raster* objects (or filenames) }
\item{\code{bands}}{Which bands (layers) of the file should be used (default is all layers)}
}}

\value{
RasterStack
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{addLayer}, \link[raster]{dropLayer}, \link[raster]{raster}, \link[raster]{brick}} }

\examples{
# file with one layer
fn <- system.file("external/test.grd", package="raster")
s <- stack(fn, fn)
r <- raster(fn)
s <- stack(r, fn) 
nlayers(s)

# file with three layers
slogo <- stack(system.file("external/rlogo.grd", package="raster")) 
nlayers(slogo)
slogo
}

\keyword{methods}
\keyword{spatial}
