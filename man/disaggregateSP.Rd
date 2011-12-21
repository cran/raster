\name{disaggregatesp}

\docType{methods}

\alias{disaggregate,SpatialPolygons-method}
\alias{disaggregate,SpatialLines-method}

\title{Disaggregate lines or polygons}

\description{
Disaggregate ('explode') method for SpatialPolygons* and SpatialLines* to create single feature polygons/lines from multi-feature polygon/lines
}

\usage{
\S4method{disaggregate}{SpatialPolygons}(x, ...)

\S4method{disaggregate}{SpatialLines}(x, ...)

}

\arguments{
  \item{x}{SpatialPolygons or SpatialLines* object}
  \item{...}{Additional arguments (none implemented)}
}


\value{
SpatialPolygons* or SpatialLines*
}

\seealso{ \code{\link{aggregate,SpatialPolygons-method}} }


\author{Robert J. Hijmans}


\keyword{methods}
\keyword{spatial}

