\name{cellValues}

\docType{methods}

\alias{cellValues}
\alias{xyValues}

\title{ Obsolete functions }

\description{
Obsolete functions. Use \code{\link[raster]{extract}} instead.
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


\seealso{ \code{\link[raster]{extract}}}


\author{Robert J. Hijmans }


\keyword{methods}
\keyword{spatial}
