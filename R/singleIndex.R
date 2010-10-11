# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("[", c("Raster","ANY", "missing"),
function(x,i,j,...,drop=TRUE) {
	
	if (! inMemory(x) ) {
		if ( ! fromDisk(x) ) {
			stop('no data associated with this RasterLayer object')
		}
	}
	
	if (missing(i)) {
		if ( inMemory(x) ) {
			return(x@data@values)
		} else {
			return(getValues(x))
		}
	}

	if (inherits(i, "RasterLayer")) {
		i <- as.logical( getValues(i) ) 
	}
	
	if (! inMemory(x) ) {
		if (canProcessInMemory(x, 2)) {
			if (length(i) > 0.5 * ncell(x)) {
				x <- readAll(x)
			}
		}
	}
	
	if ( inMemory(x) ) {
		x@data@values[i, drop=drop]
	} else {
		return(cellValues(x, i))
	}
}
)




setMethod("[", c("RasterStack","ANY", "missing"),
function(x,i,j,...,drop=TRUE) {
	
	if (missing(i)) { 
		return(getValues(x))
	}

	if (inherits(i, "RasterLayer")) {
		i <- as.logical( getValues(i) ) 
	}
	
	return(cellValues(x, i))
}
)


