# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  December 2009
# Version 0.9
# Licence GPL v3



.levelplotraster <- function(object, maxpixels=100000, xlab='', ylab='', extent=NULL, ticks=c(6,6), ...) {
	if (! require(lattice) ) { stop('cannot find the lattice package') }
	if ( dataContent(object) != 'all') { 
		if ( dataSource(object) != 'disk') {
			stop('no values associated with this RasterLayer')
		} 
		if (canProcessInMemory(object, 2)) {
			object <- readAll(object)
		}
	}

	maxpixels <- max(1, maxpixels)
	if (is.null(extent)) {
		object <- sampleRegular(object, size=maxpixels, asRaster=TRUE, corners=TRUE)
	} else {
		object <- sampleRegular(object, size=maxpixels, extent=extent, asRaster=TRUE, corners=TRUE)
	}

	xint <- ticks[2]
	xr <- xmax(object) - xmin(object)
	xint <- c(0, 1/xint * 1:xint)
	x <- xmin(object) + xint * xr
	xint <- xint * ncol(object)
	xint[1] <- 1
	x <- list('at'=xint, 'labels'=x)
	
	yint <- ticks[1]
	yr <- ymax(object) - ymin(object)
	yint <- c(0, 1/yint * 1:yint)
	y <- ymin(object) + yint * yr
	yint <- yint * nrow(object)
	yint[1] <- 1
	y <- list('at'=yint, 'labels'=y)
	
	z <- values(object, format='matrix')
	z <- t(z[nrow(z):1,])
	levelplot(z, xlab=xlab, ylab=ylab, scales=list(x=x, y=y), ...)
}
