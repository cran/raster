# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("sampleRandom")) {
	setGeneric("sampleRandom", function(x, size, ...)
		standardGeneric("sampleRandom"))
}	


setMethod('sampleRandom', signature(x='Raster'), 

function(x, size, na.rm=TRUE, ...) {
	if ( inMemory(x) ) {
		values <- getValues(x)
		if (na.rm) { values <- na.omit(values) }
		if (length(values) > size) {
			s = sampleInt(length(values), size)
			values <- values[s]
		}
	} else if ( fromDisk(x) ) {
		if (ncell(x) <= size) {
			values <- cbind(1:ncell(x), getValues(x))
			if (na.rm) { values <- na.omit(values) }
		} else {	
			if (na.rm) {
				N <- size 
			} else {
				N <- 2 * size 
			}	
			cells <- sampleInt(ncell(x), N)
			values <- cellValues(x, cells)
			if (na.rm) {
				values <- na.omit(values)
				if (length(values) >= size) {
					if (is.matrix(values)) {
						values <- values[1:size, ]
					} else {
						values <- values[1:size]
					}
				}
			}	
		}
	} else {
		stop('No values associated with the Raster object')
	}
	return(values)
}
)


setMethod('sampleRandom', signature(x='RasterStack'), 

	function(x, size, na.rm=TRUE, ...) {
		if (ncell(x) <= size) {
			values <- cbind(1:ncell(x), getValues(x))
			if (na.rm) { values <- na.omit(values) }
		} else {	
			if (na.rm) {
				N <- size 
			} else {
				N <- 2 * size 
			}	
			cells <- sampleInt(ncell(x), N)
			values <- cellValues(x, cells)
			if (na.rm) {
				values <- na.omit(values)
				if (length(values) >= size) {
					if (is.matrix(values)) {
						values <- values[1:size, ]
					} else {
						values <- values[1:size]
					}
				}
			}	
		}
		return(values)
	}
)


