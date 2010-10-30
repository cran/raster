\name{indexing}

\docType{methods}


\alias{[[,Raster,ANY,ANY-method}
\alias{[,Raster,ANY,ANY-method}
\alias{[,Raster,Spatial,missing-method}

\alias{[<-,RasterLayer-method}

\title{Get or replace values of Raster objects}

\description{

\code{object[i]} can be used to access values of a Raster* object, using cell numbers. You can also use row and column numbers as index, using \code{object[i,j]} or \code{object[i,]} or \code{object[,j]}. In addition you can supply a SpatialPolygons, SpatialLines or SpatialPoints object (as in \code{link{extract}}. If you supply a RasterLayer, its values will be used as logical (TRUE/FALSE) indices if both Raster objects have the same extent and resolution; otherwise the cell values within the extent of the RasterLayer are returned.

You can also set values of a RasterLayer object, when \code{i} is a vector of cell numbers or a Spatial* object. 
}

\section{Methods}{
\describe{
\bold{Either get values}

\code{x[i]}
\code{x[i,j]}

Arguments
\tabular{rll}{
\tab \code{x} \tab a Raster* object \cr
\tab \code{i} \tab cell number(s), row number(s), a (logical) RasterLayer, Spatial* object \cr
\tab \code{j} \tab column number(s) (only available if i is (are) a row number(s)) \cr
}

\bold{Or set values}

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

\seealso{ \code{\link{getValues}, \link{setValues}, \link{extract}} }


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

s <- stack(r,sqrt(r))
s[1:3]
}

\keyword{methods}
\keyword{spatial}
