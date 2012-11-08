# Author: Robert J. Hijmans
# Date : October 2010
# Version 1.0
# Licence GPL v3


if (!isGeneric("extract")) {
	setGeneric("extract", function(x, y, ...)
		standardGeneric("extract"))
}	



setMethod('extract', signature(x='Raster', y='vector'), 
function(x, y, ...){ 
	y <- round(y)
	if (length(y) == 2) {
		cat("note: returning values at CELL NUMBERS (not coordinates) : ", y[1], " and ", y[2], "\n")
	}
	return( .cellValues(x, y, ...) )
})
