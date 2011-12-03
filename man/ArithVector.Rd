\name{vector algebra}

\alias{+,SpatialPolygons,SpatialPolygons-method}
\alias{+,SpatialPolygonsDataFrame,data.frame-method}
\alias{-,SpatialPolygons,SpatialPolygons-method}
\alias{*,SpatialPolygons,SpatialPolygons-method}
\alias{^,SpatialPolygons,SpatialPolygons-method}

\title{Algebra (notation) for overlaying SpatialPolygons* }

\description{
These funcitons are experimental. Under development!

Given SpatialPolygons* object \code{x} and \code{y}

\code{x+y} This is the same as \code{\link{merge}(x, y)}

\code{x*y} This is the same as \code{\link{crop}(x, y)}

\code{x-y} Erase the area covered by \code{y} from \code{x}

These functions are based on functions from rgeos, with some added functionality to, among other things, avoid polygon duplication and to keep the attributes.

See \link[raster]{Arith-methods} for similar arithmetic with Raster objects
}


\details{

Comparison of terminology for polygon overlay methods used in ArcGIS and in this package (raster)
\tabular{rll}{
  \tab ArcGIS    \tab  raster \cr
  \tab dissolve  \tab  aggregate  \cr
  \tab append    \tab  merge(x, y, ..., intersect=FALSE) \cr
  \tab union     \tab  merge or "+" \cr
  \tab intersect \tab  crop or "*" \cr
  \tab erase     \tab  "-" \cr
  \tab symdif    \tab  "^" (to be done) \cr
}
 
To do: identity /update 

For spatialjoin and other spatial queries, see sp functions 'over' and 'overlay' (or raster function extract when using Raster objects)
}


\seealso{ \code{\link[raster]{merge}, \link[raster]{aggregate}}, \link[raster]{crop} }

\author{Robert J. Hijmans}

\keyword{methods}
\keyword{spatial}
