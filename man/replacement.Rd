\name{replacement}

\docType{methods}


\alias{[[,RasterLayer,ANY,ANY-method}
\alias{[[,RasterStackBrick,ANY,ANY-method}

\alias{[,Raster,Spatial,missing-method}
\alias{[,Raster,RasterLayer,missing-method}
\alias{[,Raster,Extent,missing-method}
\alias{[,Raster,numeric,numeric-method}
\alias{[,Raster,numeric,missing-method}
\alias{[,Raster,missing,numeric-method}
\alias{[,Raster,missing,missing-method}
\alias{[,Raster,logical,missing-method}

\alias{[<-,RasterLayer,numeric,numeric-method}
\alias{[<-,RasterLayer,numeric,missing-method}
\alias{[<-,RasterLayer,logical,missing-method}
\alias{[<-,RasterLayer,missing,numeric-method}
\alias{[<-,RasterLayer,missing,missing-method}
\alias{[<-,RasterLayer,RasterLayer,missing-method}
\alias{[<-,RasterLayer,Spatial,missing-method}
\alias{[<-,RasterLayer,Extent,missing-method}


\title{Extract or replace cell values of a Raster object}

\description{
These are shorthand methods that call other methods that should normally be used, such as \code{\link{getValues}, \link{extract}, \link{crop}, \link{rasterize}}. 

\code{object[i]} can be used to access values of a Raster* object, using cell numbers. You can also use row and column numbers as index, using \code{object[i,j]} or \code{object[i,]} or \code{object[,j]}. In addition you can supply a SpatialPolygons, SpatialLines or SpatialPoints object. 

If \code{drop=TRUE} (the default) cell values are returned (a vector for a RasterLayer, a matrix for a RasterStack or RasterBrick). If \code{drop=FALSE} a Raster* object is returned that has the extent covering the requested cells, and with all other non-requested cells within this extent set to \code{NA}.

If you supply a RasterLayer, its values will be used as logical (TRUE/FALSE) indices if both Raster objects have the same extent and resolution; otherwise the cell values within the extent of the RasterLayer are returned.

You can also set values of a RasterLayer object, when \code{i} is a vector of cell numbers or a Spatial* object. 

Double brackes '[[ ]]' can be used to extract one or more layers from a multi-layer object.
}

\section{Methods}{
\describe{
\bold{Get values}

\code{x[i]}

\code{x[i,j]}

Arguments
\tabular{rll}{
\tab \code{x} \tab a Raster* object \cr
\tab \code{i} \tab cell number(s), row number(s), a (logical) RasterLayer, Spatial* object \cr
\tab \code{j} \tab column number(s) (only available if i is (are) a row number(s)) \cr
\tab \code{drop} \tab If \code{TRUE}, cell values are returned. Otherwise, a Raster* object is returned \cr
}

\bold{Set values}

\code{x[i] <- value}

\code{x[i,j] <- value}

Arguments
\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{i} \tab cell number(s), row number(s), Extent, Spatial* object \cr
\tab \code{j} \tab columns number(s) (only available if i is (are) a row number(s)) \cr
\tab \code{value} \tab new cell value(s) \cr
}


}}

\seealso{ \code{\link{getValues}, \link{setValues}, \link{extract}, \link{crop}, \link{rasterize}} }


\examples{
r <- raster(ncol=10, nrow=5)
r[] <- 1:ncell(r) * 2
r[1,] <- 1
r[,1] <- 2
r[1,1] <- 3

as.matrix(r)

r[1]
r[1:10]
r[1,]
r[,1]
r[1:2, 1:2]

s <- stack(r, sqrt(r))
s[1:3]
s[[2]]
}

\keyword{methods}
\keyword{spatial}
