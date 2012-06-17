\name{plot}

\docType{methods}

\alias{plot}
\alias{plot,Raster,ANY-method}
\alias{plot,Raster,Raster-method}
\alias{plot,Extent,ANY-method}
\alias{plot,Extent,missing-method}

\title{Plot a Raster* object}

\description{
Plot (that is, make a map of) the values of a Raster* object, or make a scatterplot of their values.

Points, lines, and polygons can be drawn on top of a map using \code{plot(..., add=TRUE)}, or with functions like \code{points, lines, polygons}

See the rasterVis package for more advanced (trellis/lattice) plotting of Raster* objects.
}

\usage{
\S4method{plot}{Raster,ANY}(x, y, col=rev(terrain.colors(255)), maxpixels=500000, alpha=1, colNA=NA, add=FALSE, ext=NULL, useRaster=TRUE, interpolate=FALSE, addfun=NULL, nc, nr, ...)


\S4method{plot}{Raster,Raster}(x, y, maxpixels=100000, cex=0.1, ...)
}


\arguments{
  \item{x}{Raster* object}
  \item{y}{If \code{x} is a RasterStack or RasterBrick: integer, character (layer name(s)), or missing to select which layer(s) to plot. If missing, all RasterLayers in the RasterStack will be plotted (up to a maximum of 16). Or another Raster* object of the same extent and resolution, to produce a scatter plot of the cell values. }
  \item{col}{A color palette, i.e. a vector of n contiguous colors such as \link{rainbow}, \link{heat.colors}, \link{topo.colors},
       \link[sp]{bpy.colors} or one or your own making, perhaps using \code{\link{colorRampPalette}}}
  \item{maxpixels}{Maximum number of pixels used}
  \item{useRaster}{If \code{TRUE}, the rasterImage function is used for plotting. Otherwise the image function is used. This can be useful
       if rasterImage does not work well on your system (see note)}
  \item{alpha}{Number between 0 and 1 to set transparancy. 0 is entirely transparant, 1 is not transparant}
  \item{colNA}{The color to use for the backgournd (default is transparant)}  
  \item{add}{Logical. Add to current plot?}
  \item{ext}{An extent object to zoom in a region (see also \code{\link{zoom}} and \code{\link{crop}(x, \link{drawExtent}())}}
  \item{interpolate}{Logical. Should the image be interpolated (smoothed)?}
  \item{addfun}{Function to add additional items such as points or polygons to the plot (map). Typically containing statements like
        "points(xy); plot(polygons, add=TRUE)". This is particularly useful to add something to each map when plotting a multi-layer Raster* object.}
  \item{...}{Graphical parameters. Any argument that can be passed to \code{\link[fields]{image.plot}} and to \code{\link[graphics]{plot}}, 
        such as axes=FALSE, main='title', ylab='latitude'}
  \item{nc}{Optional. The number of columns to divide the plotting device in (when plotting mulitple layers in a RasteLayer or RasterBrick object)}
  \item{nr}{Optional. The number of rows to divide the plotting device in (when plotting mulitple layers in a RasteLayer or RasterBrick object)}
  \item{cex}{Symbol size for scatter plots}
}


\details{
Most of the code for this function was taken from image.plot (fields package). 

Raster objects with a color-table (e.g. a graphics file) are plotted according to the color table.
}


\note{
raster uses \code{\link[graphics]{rasterImage}} from the graphics package. For unknown reasons this does not work on Windows Server. On that system you may need to use argument \code{useRaster=FALSE} to get a plot.
}


\seealso{
\code{ \link[raster]{spplot}} and the \code{rasterVis} package have lattice based methods for plotting Raster* objects. 

red-green-blue plots (e.g. false color composits) can be made with \code{\link[raster]{plotRGB}}

\code{\link[raster]{text}, \link[raster]{persp}, \link[raster]{contour}, \link[raster]{pairs}, \link[raster]{hist}}
}

\author{ Robert J. Hijmans }

\examples{ 
# RasterLayer
r <- raster(nrows=10, ncols=10)
r <- setValues(r, 1:ncell(r))
plot(r)

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

# two objects, different range, one scale:
r[] <- runif(ncell(r))
r2 <- r/2
brks <- seq(0, 1, by=0.1) 
nb <- length(brks)-1 
par(mfrow=c(1,2))
plot(r, breaks=brks, col=rev(terrain.colors(nb)), lab.breaks=brks, zlim=c(0,1)) 
plot(r2, breaks=brks, col=rev(terrain.colors(nb)), lab.breaks=brks, zlim=c(0,1)) 


# breaks and labels
x <- raster(nc=10, nr=10)
x[] <- runif(ncell(x))
brk <- c(0, 0.25, 0.75, 1)
arg <- list(at=c(0.12,0.5,0.87), labels=c("Low","Med.","High"))
plot(x, col=terrain.colors(3), breaks=brk)
plot(x, col=terrain.colors(3), breaks=brk, axis.args=arg)
par(mfrow=c(1,1))

# color ramp
plot(x, col=colorRampPalette(c("red", "white", "blue"))(255))

# adding random points to the map
xy <- cbind(-180 + runif(10) * 360, -90 + runif(10) * 180)
points(xy, pch=3, cex=5)

# for SpatialPolygons do
# plot(pols, add=TRUE)

# adding the same points to each map of each layer of a RasterStack
fun <- function() {
	points(xy, cex=2)
	points(xy, pch=3, col='red')
}
plot(s, addfun=fun)

}

\keyword{methods}
\keyword{spatial}

