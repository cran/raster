# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



.writeGDALall <- function(x, filename, options=NULL, ...) {
	
	x <- .startGDALwriting(x, filename, options, ...)
	x <- writeValues(x, values(x), start=1)
	.stopGDALwriting(x)

}
	
