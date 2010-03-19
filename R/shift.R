# Author: Robert J. Hijmans,  r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("shift")) {
	setGeneric("shift", function(object, ...)
		standardGeneric("shift"))
}	


setMethod('shift', signature(object='Raster'), 
	function(object, x=0, y=0, filename='', ...) {
		object@extent@xmin <- object@extent@xmin + x
		object@extent@ymin <- object@extent@ymin + y
		object@extent@xmax <- object@extent@xmax + x
		object@extent@ymax <- object@extent@ymax + y
		if (filename != '') {
			object <- saveAs(object, filename=filename, ...)
		}
		return(object)
	}
)
