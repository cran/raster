# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2010
# Version 1.0
# Licence GPL v3


'gain<-' <- function(x, value) {
	value <- as.numeric(value[1])
	if (inherits(x, 'RasterStack')) {
		x@layers <- lapply( x@layers, function(z) { z@data@gain = value; return(z)} )
	} else {
		x@data@gain <- value
	}
	return(x)
}


gain <- function(x) {
	if (inherits(x, 'RasterStack')) {
		r <- sapply( x@layers, function(z) { z@data@gain } )
	} else {
		r <- x@data@gain 
	}
	return(r)
}


'offs<-' <- function(x, value) {
	value <- as.numeric(value[1])
	if (inherits(x, 'RasterStack')) {
		x@layers <- lapply( x@layers, function(z) { z@data@offset = value; return(z) } )
	} else {
		x@data@offset <- value
	}
	return(x)
}


offs <- function(x) {
	if (inherits(x, 'RasterStack')) {
		r <- sapply( x@layers, function(z) { z@data@offset } )
	} else {
		r <- x@data@offset 
	}
	return(r)
}

