# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("image")) {
	setGeneric("image", function(x,...)
		standardGeneric("image"))
}	

setMethod("image", signature(x='RasterLayer'), 
	function(x, maxpixels=500000, ...)  {
		x <- sampleRegular(x, maxpixels, asRaster=TRUE)
		y <- yFromRow(x, nrow(x):1)
		value <- t((getValues(x, format='matrix'))[nrow(x):1,])
		x <- xFromCol(x,1:ncol(x))
		image(x=x, y=y, z=value,  ...)
	}
)


setMethod("image", signature(x='RasterStackBrick'), 
	function(x, y=1, maxpixels=100000, ...)  {
		if (y < 1) { y <- 1 }
		if (y > nlayers(x)) { y <- nlayers(x) }
		image(x=x, y=y, maxpixels=maxpixels, ...)
	}	
)

