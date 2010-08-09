# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("[[", c("RasterStackBrick","ANY", "ANY"),
function(x,i,j,...,drop=TRUE) {
	if (!missing(i)) {
		if ( inherits(i, "RasterLayer")) {
			i <- as.logical( getValues(i) ) 
		}
	}
	if ( missing(j) ) {
		cells <- cellFromRow(x, i)
	} else if (missing(i)) {
		cells <- cellFromCol(x, j)
	} else {
		cells <- cellFromRowColCombine(x, i, j)
	}
	if ( inMemory(x) ) {
		return(x@data@values[i=cells, , drop=drop])
	} else {
		return(cellValues(x, cells))
	}
}
)

