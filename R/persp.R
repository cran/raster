# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("persp")) {
	setGeneric("persp", function(x,...)
		standardGeneric("persp"))
}	

setMethod("persp", signature(x='RasterLayer'), 
	function(x, maxpixels=100000, ...)  {
		if (dataContent(x) != 'all') { 
#	to do: should  test if can read, else sample
			if (canProcessInMemory(x, 2)) {
				x <- readAll(x) 
			} else {
				x <- sampleRegular(x, size=maxpixels, asRaster=TRUE, corners=TRUE)
			}
		}
		value <- t((values(x, format='matrix'))[nrow(x):1,])
		y <- yFromRow(x, nrow(x):1)
		x <- xFromCol(x,1:ncol(x))
		persp(x=x, y=y, z=value, ...)
	}
)

setMethod("persp", signature(x='RasterStackBrick'), 
	function(x, y=1, maxpixels=1000, ...)  {
		if (y < 1) { y <- 1 }
		if (y > nlayers(x)) { y <- nlayers(x) }
		persp(x=x, y=y, maxpixels=maxpixels, ...)
	}	
)

