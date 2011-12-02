# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : August 2010
# Version 0.9
# Licence GPL v3



if (!isGeneric("text")) {
	setGeneric("text", function(x, ...)
		standardGeneric("text"))
}	


setMethod('text', signature(x='RasterLayer'), 
	function(x, digits=0, ...) {
		x = rasterToPoints(x, ..., spatial=FALSE)
		text(x[,1], x[,2], as.character(round(x[,3], digits=digits) ))
	}
)

