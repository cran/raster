# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3


.writeGDALall <- function(x, filename, options=NULL, ...) {
	if (nlayers(x) > 1) {
		y <- brick(x, values=FALSE)
		x <- getValues(x)
		stat <- t(apply(x, 2, function(z, ...) cbind(mean(z, na.rm=TRUE), sd(z, na.rm=TRUE))))
	} else {
		y <- raster(x)
		x <- getValues(x)
		stat <- cbind(mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
	}
	y <- .startGDALwriting(y, filename, options, ...)
	x <- writeValues(y, x, start=1)
	.stopGDALwriting(x, stat)
}
	
