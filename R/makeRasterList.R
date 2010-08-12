# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3

.addToList <- function(x, r, compare, giveError) {
	if (class(r) == 'character') {
		r <- raster(r)
		# or r <- unstack(stack(r, -1)) ???
		if (compare & length(x)>0) { compare(x[[1]], r)  }
		return( c(x, r) )
	} else if (! extends(class(r), 'Raster')) {
		if (giveError) {
			stop('... arguments must be a filename or objects that extend the Raster class')
		} else {
			return(x)
		}
	} else if (class(r) == 'RasterLayer') {
		if (compare & length(x)>0) { compare(x[[1]], r)  }
		return( c(x, r) )	
	} else {
		if (compare & length(x)>0) { compare(x[[1]], r)  }
		return( c(x, unstack(r)) )
	} 
}



.makeRasterList <- function(..., compare=FALSE, giveError=FALSE, keepone=FALSE) {
	arg <- list(...)
	x <- list()
	for (i in seq(along=arg)) {
		if (class(arg[[i]]) == 'list') {
			for (j in seq(along=arg[[i]])) {
				x <- .addToList(x, arg[[i]][[j]], compare, giveError) 
			}
		} else {
			x <- .addToList(x, arg[[i]], compare, giveError) 
		}
	}
	for (i in rev(seq(along=x))) {
		if (! inMemory(x[[i]])  &  ! fromDisk(x[[i]]) ) {
			if (length(x) > 1 ) {
				x <- x[[-i]] 
				warning('RasterLayer with no data ignored')
			} else if (keepone==FALSE ) {
				x <- list()
				warning('RasterLayer with no data ignored')
			}
		} 
	}		
	return(x)
}

