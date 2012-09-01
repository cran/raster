# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("shift")) {
	setGeneric("shift", function(object, ...)
		standardGeneric("shift"))
}	


setMethod('shift', signature(object='Raster'), 
	function(object, x=0, y=0, filename='', ...) {
		x <- as.numeric(x[1])
		y <- as.numeric(y[1])
		stopifnot(!is.na(x) | !is.na(y))
		e <- object@extent
		e@xmin <- e@xmin + x
		e@ymin <- e@ymin + y
		e@xmax <- e@xmax + x
		e@ymax <- e@ymax + y
		object@extent <- e
		if (filename != '') {
			object <- writeRaster(object, filename=filename, ...)
		}
		return(object)
	}
)


