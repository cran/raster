# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("[[", c("RasterLayer","ANY", "ANY"),
function(x,i,j,...,drop=TRUE) {

	if (!missing(i) && class(i) == "RasterLayer") {
		i <- as.logical( getValues(i) ) 
	}

	if (! inMemory(x) ) {
		if ( ! fromDisk(x) ) {
			stop('no data associated with this RasterLayer object')
		}
	}

	if ( inMemory(x) ) {
		m <- matrix(x@data@values, nrow(x), ncol(x), byrow=TRUE)
		rm(x)
		return(m[i=i, j=j, drop=drop])
	} else {
		if ( missing(j) ) {
			cells <- cellFromRow(x, i)
		} else if (missing(i)) {
			cells <- cellFromCol(x, j)
		} else {
			cells <- cellFromRowColCombine(x, i, j)
		}
		return(cellValues(x, cells))
	}
}
)

