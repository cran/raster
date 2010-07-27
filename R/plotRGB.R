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
function(x, r=1, g=2, b=3, scale=255, maxpixels=500000, extent=NULL, interpolate=FALSE, axes=TRUE, xlab='', ylab='', asp, ...) { 
	
	if (!axes) par(plt=c(0,1,0,1))
	
	# rasterImage is new in R 2.11
	v <- version
	major <- as.numeric( v$major )
	minor <- as.numeric( v$minor )
	if (major < 2 | (major == 2 & minor < 11)) stop('You need R version 2.11 or higher to use this function')

 	if (missing(asp)) {
		if (.couldBeLonLat(x)) {
			ym <- mean(x@extent@ymax + x@extent@ymin)
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
	
	RGB <- na.omit(cbind(getValues(r), getValues(g), getValues(b)))
	
	naind <- as.vector(attr(RGB, "na.action"))
	if (!is.null(naind)) {
		z <- rep( "#000000", times=ncell(r))
		z[-naind] <- rgb(RGB[,1], RGB[,2], RGB[,3], max=scale)
	} else {
		z <- rgb(RGB[,1], RGB[,2], RGB[,3], max=scale)
	}
	
	z <- matrix(z, nrow=nrow(r), ncol=ncol(r), byrow=T)

	require(grDevices)
	bb <- as.vector(t(bbox(r)))
	plot(c(bb[1], bb[2]), c(bb[3], bb[4]), type = "n", xlab=xlab, ylab=ylab, asp=asp, axes=FALSE, ...)
	rasterImage(z, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate, ...)
	
	if (axes) {
		xticks <- axTicks(1, c(xmin(r), xmax(r), 4))
		yticks <- axTicks(2, c(ymin(r), ymax(r), 4))
		if (xres(r) %% 1 == 0) xticks = round(xticks)
		if (yres(r) %% 1 == 0) yticks = round(yticks)
		axis(1, at=xticks)
		axis(2, at=yticks, las = 1)
		axis(3, at=xticks, labels=FALSE, lwd.ticks=0)
		axis(4, at=yticks, labels=FALSE, lwd.ticks=0)
	}	
}
)

