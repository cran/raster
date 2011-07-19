# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009
# Version 0.9
# Licence GPL v3


.plotraster2 <- function(object, col=rev(terrain.colors(25)), maxpixels=100000, axes=TRUE, xlab='', ylab='', ext=NULL, asp, xlim, ylim, ...) {
 
 	if (missing(asp)) {
		if (.couldBeLonLat(object, warnings=FALSE)) {
#			ym <- mean(object@extent@ymax + object@extent@ymin)
#			asp <- min(5, 1/cos((ym * pi)/180))
			asp = NA
		} else {
			asp = 1
		}		
	}

	if ( ! inMemory(object) ) { 
		if (  !  fromDisk(object) ) {
			stop('no values associated with this RasterLayer')
		} 
	}

	maxpixels <- max(1, maxpixels)

	if (is.null(ext)) {
		ext <- extent(object)
	} else  { 
		ext <- intersectExtent(extent(object), ext) 
	}
	
	if (!missing(xlim)) { 
		if (xlim[1] >= xlim[2]) stop('invalid xlim')
		if (xlim[1] < ext@xmax) ext@xmin <- xlim[1]
		if (xlim[2] > ext@xmin) ext@xmax <- xlim[2]
	} 
	if (!missing(ylim)) { 
		if (ylim[1] >= ylim[2]) stop('invalid ylim')
		if (ylim[1] < ext@ymax) ext@ymin <- ylim[1]
		if (ylim[2] > ext@ymin) ext@ymax <- ylim[2]
	} 
	
#	leg <- object@legend
	object <- sampleRegular(object, size=maxpixels, ext=ext, asRaster=TRUE)
	.rasterImagePlot(object, col=col, axes=axes, xlab=xlab, ylab=ylab, asp=asp, ...)	
}


