# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3


.writeGDALall <- function(x, filename, options=NULL, ...) {
	if (nlayers(x) > 1) {
		y <- brick(x, values=FALSE)
	} else {
		y <- raster(x)
	}
	y <- .startGDALwriting(y, filename, options, ...)
	y <- writeValues(y, values(x), start=1)
	.stopGDALwriting(y)
}
	
