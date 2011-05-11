\name{plot}

\docType{methods}

\alias{plot}
\alias{plot,RasterLayer,missing-method}
\alias{plot,RasterStackBrick,ANY-method}
\alias{plot,RasterLayer,RasterLayer-method}
\alias{plot,Extent,ANY-method}
\alias{plot,Extent,missing-method}
\alias{text}
\alias{text,RasterLayer-method}

\title{Plot a Raster* object}

\description{
Plot (that is, make a map of) the values of a Raster* object, or make a scatterplot of their values.

Points, lines, and polygons can be drawn on top of a map using \code{plot(..., add=TRUE)}, or functions like \code{points, lines, polygons}

\code{text} plots a textual (rather than color) representation of values of the cells on top of the cells. 
}


\section{Methods}{
\describe{
\bold{x=RasterLayer}

\code{plot(x, col=rev(terrain.colors(25)), maxpixels=100000, axes = TRUE, xlab="", ylab="", alpha=1, ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{col}  \tab  A color palette, i.e. a vector of n contiguous colors such as \code{rainbow}, \code{heat.colors}, and \code{topo.colors}, or one or your own making \cr
\tab \code{maxpixels}  \tab  Maximum number of pixels used for the map  \cr
\tab \code{alpha}  \tab  Number between 0 and 1 to set transparancy. 0 is entirely transparant, 1 is not transparant  \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[fields]{image.plot}} and to \code{\link[graphics]{plot}}  \cr
}


If \code{x} has a 'Color table', the default all to \code{plot} is:

\code{plot(x, maxpixels=500000, extent=NULL, interpolate=FALSE,  axes=FALSE, xlab="", ylab="", ...)}


\tabular{rll}{
\tab \code{extent}  \tab  An extent object to zoom in a region \cr
\tab \code{interpolate} \tab Logical. Should the image be interpolated?  \cr 
\tab \code{axes} \tab Logical. Should axes be drawn?  \cr 
\tab \code{...} \tab Graphical parameters \cr
}



\bold{x=RasterStack or RasterBrick, y=numeric or missing}

\code{plot(x, col=rev(terrain.colors(25)), subsample=TRUE, maxpixels=100000, alpha=1, axes=TRUE, xlab="", ylab="", ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterStack or RasterBrick object \cr
\tab \code{y} \tab The index of a RasterLayer in a RasterStack to be plotted. Either a single number between 1 and nlayers(rstack), a vector of numbers, or missing. If y is missing, all RasterLayers in the RasterStack will be plotted.\cr
\tab \code{col}  \tab  A color palette, i.e. a vector of n contiguous colors such as \code{rainbow}, \code{heat.colors}, and \code{topo.colors}, or one or your own making \cr
\tab \code{maxpixels}  \tab  Maximum number of pixels used for the map  \cr
\tab \code{alpha}  \tab  Number between 0 and 1 to set transparancy. 0 is entirely transparant, 1 is not transparant  \cr
\tab \code{axes}  \tab  see \code{?image} \cr
\tab \code{xlab}  \tab  see \code{?image} \cr
\tab \code{ylab}  \tab  see \code{?image} \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[fields]{image.plot}} and to \code{\link[graphics]{plot}}  \cr
}



\bold{x=RasterLayer, y=RasterLayer}
Produces a scatter plot of the values of x against those of y. 
\code{plot(x, y, maxpixels=100000, cex=0.1, ...) }

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{y} \tab a RasterLayer object \cr
\tab \code{maxpixels}  \tab  Maximum number of pixels in the map display \cr
\tab \code{cex}  \tab  point size \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[graphics]{plot}}  \cr
}

\bold{x=Extent, y="ANY"}
Draws the (rectangular) bounding box of a raster object. 
\code{plot(ext) }

\tabular{rll}{
\tab \code{ext} \tab a Extent object \cr
}

}}


\details{
Most of the code for this function was taken from image.plot (fields package). 
Raster objects with a color-table (e.g. a graphics file) will be plotted according to the color table.
}

\seealso{
  \code{ \link[raster]{plotRGB}, \link[raster]{plot3D}, \link[raster]{persp}, \link[raster]{contour}, \link[raster]{pairs}, \link[raster]{hist}}  
}

\author{ Robert J. Hijmans }

\examples{ 
# RasterLayer
r <- raster(nrows=10, ncols=10)
r <- setValues(r, 1:ncell(r))
plot(r)
text(r)

e <- extent(r)
plot(e, add=TRUE, col='red', lwd=4)
e <- e / 2
plot(e, add=TRUE, col='red')


# Scatterplot of 2 RasterLayers
r2 <- sqrt(r)
plot(r, r2)

# Multi-layer object (RasterStack / Brick)
s <- stack(r, r2, r/r)
plot(s, 2)
plot(s)
}

\keyword{methods}
\keyword{spatial}

