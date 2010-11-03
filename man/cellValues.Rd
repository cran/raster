\name{cellValues}

\docType{methods}

\alias{cellValues}
\alias{xyValues}

\title{ Extract values for one or more cells / points }

\description{
Obsolete functions. Use \code{\link[raster]{extract}} in stead.
}

\usage{
cellValues(x, cells, ...)
xyValues(object, xy, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{object}{Raster* object}
 \item{xy}{Coordinates (e.g. a matrix of two columns or a SpatialPoints* object)}
 \item{cells}{vector of cell numbers, cell numbers should be between 1 and ncells(x)}
 \item{...}{Additional arguments, see Details}
}


\value{
a vector of cell values for a RasterLayer or or a matrix of values for a RasterStack or RasterBrick
}

\seealso{ \code{\link[raster]{extract}}}


\author{Robert J. Hijmans }



\keyword{methods}
\keyword{spatial}
