# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2010
# Version 0.9
# Licence GPL v3

# based on functions in the pixmap package by Friedrich Leisch

if (!isGeneric("plotRGB")) {
	setGeneric("plotRGB", function(x, ...)
		standardGeneric("plotRGB"))
}	

setMethod("plotRGB", signature(x='RasterStackBrick'), 

function(x, r=1, g=2, b=3, scale=255, maxpixels=100000, extent=NULL, axes=TRUE, xlab='', ylab='', asp, ...) { 
	
 	if (missing(asp)) {
		if (.couldBeLonLat(x)) {
			ym <- mean(object@extent@ymax + object@extent@ymin)
			asp <- min(5, 1/cos((ym * pi)/180))
			asp = NA
		} else {
			asp = 1
		}		
	}

	r <- sampleRegular(raster(x,r), maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	g <- sampleRegular(raster(x,g), maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	b <- sampleRegular(raster(x,b), maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	scale = as.vector(scale)[1]
	z <- rgb(getValues(r), getValues(g), getValues(b), max=scale)
	col <- unique(z)
	z <- match(z, col)

	x <- xFromCol(r, 1:ncol(r))
	y <- yFromRow(r, nrow(r):1)
	z <- matrix(z, nrow=nrow(r), ncol=ncol(r), byrow=T)
	z <- t(z[nrow(z):1,])

	xticks <- axTicks(1, c(xmin(r), xmax(r), 4))
	yticks <- axTicks(2, c(ymin(r), ymax(r), 4))
	if (xres(r) %% 1 == 0) xticks = round(xticks)
	if (yres(r) %% 1 == 0) yticks = round(yticks)
	
	image(x=x, y=y, z=z,  col=col, axes=FALSE, xlab=xlab, ylab=ylab, asp=asp, ...)
	axis(1, at=xticks)
	axis(2, at=yticks, las = 1)
	axis(3, at=xticks, labels=FALSE, lwd.ticks=0)
	axis(4, at=yticks, labels=FALSE, lwd.ticks=0)
}
)

