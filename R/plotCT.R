# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  July 2010
# Version 0.9
# Licence GPL v3


# adapted from .isSDI in the svMisc package, copyright Philippe Grosjean,
# Romain Francois & Kamil Barton (via the sp package)
.warnIfSDI <- function()	 {
# have we been here before?
	d <- getOption('rasterImageSDIWarningGiven')
	if (is.null(d)) {
# Check if Rgui was started in SDI mode 
# 1) First is it Rgui?
		options('rasterImageSDIWarningGiven' = TRUE)
		if (.Platform$OS.type == "windows") {
			if (.Platform$GUI[1] == "Rgui") { 
# RGui SDI mode: returns "R Console", in MDI mode: returns "RGui"
				if (getIdentification() == "R Console")  {
					warning ("Because of a bug in SDI raster handling your R graphics window may stop displaying output. When this happens, colse the graphics window and plot again.")
				}
			}
		}
	}
}


.plotCT <- function(x, maxpixels=500000, extent=NULL, interpolate=FALSE, axes=FALSE, xlab='', ylab='', asp, ...) { 
# plotting with a color table
	
	.warnIfSDI()

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


