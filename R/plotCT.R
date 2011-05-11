# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  July 2010
# Version 0.9
# Licence GPL v3


.plotCT <- function(x, maxpixels=500000, extent=NULL, interpolate=FALSE, axes, xlab='', ylab='', asp, ...) { 
# plotting with a color table
	
	if (missing(axes)) {
		axes <- FALSE
	} 
	if (!axes) par(plt=c(0,1,0,1))
 	if (missing(asp)) {
		if (.couldBeLonLat(x)) {
			ym <- mean(x@extent@ymax + x@extent@ymin)
			asp <- min(5, 1/cos((ym * pi)/180))
			asp = NA
		} else {
			asp = 1
		}		
	}

	coltab <- x@legend@colortable
	x <- sampleRegular(x, maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	z <- getValues(x)
	
	if (NCOL(coltab) == 2) {
		# not implemented
		z <- as.numeric(cut(z, coltab[,1]))
		coltab <- as.vector(coltab[,2])
	}
	
	z <- z + 1
	z[is.na(z)] <- 1
	if (! is.null(coltab) ) {
		z <- matrix(coltab[z], nrow=nrow(x), ncol=ncol(x), byrow=T)
		z <- as.raster(z)
	} else {
		z <- matrix(z, nrow=nrow(x), ncol=ncol(x), byrow=T)
		z <- as.raster(z, max=max(z)) #, na.rm=TRUE))
	}

	require(grDevices)
	bb <- as.vector(t(bbox(x)))
	plot(c(bb[1], bb[2]), c(bb[3], bb[4]), type = "n", xlab=xlab, ylab=ylab, asp=asp, axes=axes, ...)
	rasterImage(z, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate, ...)
}


