\name{obsolete}

\alias{polygonValues}
\alias{lineValues}
\alias{xyValues}

\title{ Obsolete functions }

\description{
Obsolete functions. Use \code{\link[raster]{extract}} instead.
}

\usage{
polygonValues(p, x, ...)
lineValues(lns, x, ...)
xyValues(object, xy, ...)
}

\arguments{
\item{p}{SpatialPolygons* object}
\item{lns}{SpatialLines* object}
\item{x}{Raster* object}
 \item{object}{Raster* object}
 \item{xy}{Coordinates (e.g. a matrix of two columns or a SpatialPoints* object)}
\item{...}{additional arguments}
}

\value{
A list
}

\seealso{ \code{\link[raster]{extract} } }

\author{Robert J. Hijmans }


\keyword{methods}
\keyword{spatial}
