\name{text}

\docType{methods}

\alias{text}
\alias{text,RasterLayer-method}
\alias{text,RasterStackBrick-method}
\alias{text,SpatialPoints-method}
\alias{text,SpatialPolygons-method}

\title{Add labels to a map}

\description{
Plots labels, that is a textual (rather than color) representation of values, on top an existing plot (map). 
}

\usage{
\S4method{text}{RasterLayer}(x, labels, digits=0, fun=NULL, ...)

\S4method{text}{RasterStackBrick}(x, labels, digits=0, fun=NULL, ...)

\S4method{text}{SpatialPolygons}(x, labels, ...)

\S4method{text}{SpatialPoints}(x, labels, ...)
}

\arguments{
  \item{x}{Raster*, SpatialPoints* or SpatialPolygons* object}
  \item{labels}{Character. Optional. Vector of labels with \code{length(x)} or a variable name from \code{names(x)}}
  \item{digits}{Integer. how many digits should be used?}
  \item{fun}{Function to subset the values plotted (as in \code{\link{rasterToPoints}})}
  \item{...}{Additional arguments to pass to graphics function \code{\link[graphics]{text}} }
}

\seealso{
\code{\link[graphics]{text}, \link[raster]{plot}}
}



\examples{ 
r <- raster(nrows=4, ncols=4)
r <- setValues(r, 1:ncell(r))
plot(r)
text(r)

plot(r, col=bpy.colors(5))
text(r, fun=function(x){x<5 | x>12}, col=c('red', 'white'), vfont=c("sans serif", "bold"), cex=2)
}

\keyword{methods}
\keyword{spatial}

