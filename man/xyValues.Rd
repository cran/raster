\name{xyValues}
\docType{methods}

\alias{xyValues}
\alias{xyValues,Raster,vector-method}
\alias{xyValues,Raster,SpatialPoints-method}
\alias{xyValues,Raster,data.frame-method}
\alias{xyValues,RasterLayer,matrix-method}
\alias{xyValues,RasterStack,matrix-method}
\alias{xyValues,RasterBrick,matrix-method}


\title{ Extract values at xy coordinates }

\description{
These methods return the values of a Raster* object for the cells in which a set of points fall. 

Values can be extracted through bilinear interpolation of values of the four cells nearest to each point. 

It is also possible to supply a buffer to select values for cells within a certain distance around each point (see Details).
}

\usage{
xyValues(object, xy, ...)
}

\arguments{
\item{object}{Raster* object}
\item{xy}{Coordinates. A \code{n*2} matrix or dataframe with the first column having the x values, and the second column has the y values.
It can also be a \code{\link[sp]{SpatialPoints}*} object, or a vector of length 2}
\item{...}{Additional arguments. See Details}
}


\details{
These are the additional arguments that can be suplied to this function:

1) \code{method}. If \code{method='simple'} (the default), values for the cell a point falls in are returned. The 
alternative is \code{method='bilinear'}, in which case the returned values are interpolated from the values of the four nearest raster cells.

2) \code{buffer}. The radius of a buffer around each point for which to extract cell values. 
It can be a single value, or a vector of the length of the number of points. 
If the data are not projected (latitude/longitude), the unit should be meters. Otherwise it should be in map-units (typically also meters).

3) \code{fun}. Only useful when non-zero buffers are used. The values of all the cells are summarized with the function. The function should 
take a single numeric vector as argument and return a single values (e.g. mean, min or max). 

4) \code{na.rm}. Only useful when \code{fun} is supplied. If \code{na.rm=TRUE} (the default value), NA values are removed before fun is applied. 
}


\value{
A vector (object is a RasterLayer) or a matrix (object is a RasterStack or RasterBrick); Or a list if a buffer is supplied without a function.
}

\seealso{ \code{\link[raster]{cellValues}}, \code{\link[raster]{polygonValues}}, \code{\link[raster]{focalValues}} }

\author{Robert J. Hijmans }

\examples{
r <- raster()
r[] <- 1:ncell(r)
xy <- cbind(-50, seq(-80, 80, by=20))
xyValues(r, xy)
xyValues(r, xy, method='bilinear')

# examples with a buffer
xyValues(r, xy[1:3,], buffer=100000)
xyValues(r, xy[1:3,], buffer=100000, fun=mean)

## illustrating the varying size of a buffer (expressed in meters) on a longitude/latitude raster
 z <- xyValues(r, xy, buffer=1000000)
 s <- raster(r)
 for (i in 1:length(z)) { s[z[[i]]] <- i }
## compare with raster that is not longitude/latitude
 projection(r) <- NA 
 xy[,1] <- 50
 z <- xyValues(r, xy, buffer=8)
 for (i in 1:length(z)) { s[z[[i]]] <- i }
 plot(s)
# library(maptools)
# data(wrld_simpl)
# plot(wrld_simpl, add=TRUE)
}

\keyword{methods}
\keyword{spatial}
