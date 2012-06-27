# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("image")) {
	setGeneric("image", function(x,...)
		standardGeneric("image"))
}	

setMethod("image", signature(x='RasterLayer'), 
	function(x, maxpixels=500000, useRaster=TRUE, ...)  {
#		coltab <- x@legend@colortable
#		if (is.null(coltab) | length(coltab) == 0 | is.null(list(...)$col)) {
#			colortab <- FALSE		
#		}
		x <- sampleRegular(x, maxpixels, asRaster=TRUE, useGDAL=TRUE)
		y <- yFromRow(x, nrow(x):1)
		value <- t(as.matrix(x)[nrow(x):1,])
		x <- xFromCol(x,1:ncol(x))
#		if (colortab) {
#			image(x=x, y=y, z=value, col=coltab[value], useRaster=useRaster, ...)
#		} else {
		image(x=x, y=y, z=value, useRaster=useRaster, ...)			
#		}
	}
)


setMethod("image", signature(x='RasterStackBrick'), 
	function(x, y=1, maxpixels=100000, useRaster=TRUE, ...)  {
		if (y < 1) { y <- 1 }
		if (y > nlayers(x)) { y <- nlayers(x) }
		image(x=x, y=y, maxpixels=maxpixels, useRaster=useRaster, ...)
	}	
)

