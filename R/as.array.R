# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2010
# Version 1.0
# Licence GPL v3


setMethod('as.array', signature(x='RasterLayer'), 
function(x, maxpixels, ...) {
	if (!hasValues(x)) { stop("'x' has no values") }
	if (! missing(maxpixels)) {
		x <- sampleRegular(x, maxpixels, asRaster=TRUE)
	}
	x <- array(as.matrix(x), c(dim(x), 1))
	x
} )

setMethod('as.array', signature(x='Raster'), 
function(x, maxpixels, ...) {
	if (!hasValues(x)) { stop("'x' has no values") }
	if (! missing(maxpixels)) {
		x <- sampleRegular(x, maxpixels, asRaster=TRUE)
	}
	dm <- dim(x)
	ar <- array(NA, dm)
	x <- getValues(x)
	for (i in 1:dm[3]) {
		ar[,,i] <- matrix(x[,i], nrow=dm[1])
	}
	ar
} )

