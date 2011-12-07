\name{mergesp2}

\docType{methods}

\alias{merge,SpatialPolygons,SpatialPolygons-method}

\title{ 
Spatially merge Spatial* objects
}

\description{
Merge (append and intersect) SpatialPolygon* objects. Overlapping polygons are (by default) intersected, and tabular attributes are joined.
}

\usage{
\S4method{merge}{SpatialPolygons,SpatialPolygons}(x, y, ..., intersect=TRUE)
}

\arguments{
  \item{x}{SpatialPolygons* object}
  \item{y}{SpatialPolygons* object}
  \item{...}{Additional SpatialPolygons* objects}
  \item{intersect}{Logical. If \code{FALSE}, objects are appended, without intersecting}
}

\value{
SpatialPolygons*
}

\author{Robert J. Hijmans}

\seealso{
The \code{\link{merge,Spatial,data.frame-method}} for merging a data.frame with attributes of Spatial objects

The \code{\link{merge,Raster,Raster-method}} for merging Raster objects
}


\keyword{methods}
\keyword{spatial}

