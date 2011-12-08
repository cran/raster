\name{mergesp1}

\docType{methods}

\alias{merge,Spatial,data.frame-method}

\title{ 
Merge a Spatial* object with a data.frame
}

\description{
Merge a Spatial* object with a data.frame (i.e. merging of non-spatial attributes).
}

\usage{
\S4method{merge}{Spatial,data.frame}(x, y, by=intersect(names(x), names(y)), by.x=by, by.y=by, all.x=TRUE, suffixes = c(".x",".y"), incomparables=NULL, ...)
}

\arguments{
  \item{x}{Spatial* object}
  \item{y}{data.frame}
  \item{by, by.x, 
  by.y}{specifications of the common columns. See 'Details' in (base) \code{\link[base]{merge}}}.
  \item{all.x}{logical; if TRUE, then the returned object will have all rows of x, even those that has no matching row in y. These rows will have NAs in those columns that are usually filled with values from y}
  \item{suffixes}{character(2) specifying the suffixes to be used for making non-by names() unique.}
  \item{incomparables}{values which cannot be matched. See \code{\link[base]{match}}.}
  \item{...}{arguments to be passed to or from methods.}
} 


\value{
Spatial*
}

\author{Robert J. Hijmans}

\seealso{
The \code{\link{merge,SpatialPolygons,SpatialPolygons-method}} for appending and perhaps spatially intersecting polygons

The \code{\link{merge,Raster,Raster-method}} for merging Raster objects
}


\keyword{methods}
\keyword{spatial}

