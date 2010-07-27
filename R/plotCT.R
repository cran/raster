# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  July 2010
# Version 0.9
# Licence GPL v3


.plotCT <- function(x, maxpixels=500000, extent=NULL, interpolate=FALSE, axes=FALSE, xlab='', ylab='', asp, ...) { 
	
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



	r <- sampleRegular(x, maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	z <- getValues(r) + 1
	z[is.na(z)] <- 1
	
	coltab <- x@legend@colortable
	
	if (! is.null(coltab) ) {
		z <- matrix(coltab[z], nrow=nrow(r), ncol=ncol(r), byrow=T)
		z <- as.raster(z)
	} else {
		z <- matrix(z, nrow=nrow(r), ncol=ncol(r), byrow=T)
		z <- as.raster(z, max=max(z)) #, na.rm=TRUE))
	}

	require(grDevices)
	bb <- as.vector(t(bbox(r)))
	plot(c(bb[1], bb[2]), c(bb[3], bb[4]), type = "n", xlab=xlab, ylab=ylab, asp=asp, axes=axes, ...)
	rasterImage(z, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate, ...)
}


