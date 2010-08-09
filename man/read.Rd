\name{read}

\alias{readAll,RasterLayer-method}
\alias{readAll,RasterStack-method}
\alias{readAll,RasterBrick-method}

\alias{readAll}

\title{Read values from disk}

\description{
Read all values from a raster file associated with a Raster* object into memory. In most cases \code{\link[raster]{getValues}} or \code{\link[raster]{getValuesBlock}} is more approprate as readAll will fail when there is no file associated with the RasterLayer (values may only exist on memory). 
}


\usage{
readAll(object)
}


\arguments{
  \item{object}{a Raster* object}
}


\note{
After read* the values are accessible with the "values()" function.
 
Functions readRow, readRows and readBlock were depracted, but are (for the time being) still available as raster:::..readRow, raster:::..readRows and raster:::..readBlock
}

\seealso{ \code{\link[raster]{getValues}}, \code{\link[raster]{getValuesBlock}}, \code{\link[raster]{xyValues}}, \code{\link[raster]{cellValues}}}

\author{Robert J. Hijmans}

\examples{
r <- raster(system.file("external/test.grd", package="raster"))
r <- readAll(r)
}

\keyword{classes}
\keyword{spatial}
