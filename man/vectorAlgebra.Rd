\name{vectorAlgebra}

\alias{+,SpatialPolygons,SpatialPolygons-method}
\alias{-,SpatialPolygons,SpatialPolygons-method}
\alias{*,SpatialPolygons,SpatialPolygons-method}

\title{Algebra (notation) with polygons}

\description{
Under development!
Given SpatialPolygons* object \code{x} and \code{y}

\code{x + y}  --  same as \code{\link{merge}}

\code{x - y}  --  same as \code{\link[rgeos]{gDifference}}

\code{x * y}  --  same as \code{\link{crop}}

Using functions from rgeos, with some added functionality to keep attributes.
}

\seealso{ \code{\link[raster]{merge}, \link[raster]{aggregate}}, \link[raster]{crop} }

\author{Robert J. Hijmans}

\keyword{methods}
\keyword{spatial}
