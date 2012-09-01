# Author: Robert J. Hijmans
# Date :  June 2008
# Version 1.0
# Licence GPL v3


'NAvalue<-' <- function(x, value) {
	if (inherits(x, 'RasterStack')) {
		nl <- nlayers(x)
		if (length(value) == 1) {
			value <- rep(value[[1]], nl)
		} else {
			v <- vector(length=nl)
			v[] <- as.vector(value)
			value <- v
		}
		for (i in 1:nl) {
			x@layers[[i]]@file@nodatavalue <- value[i]
		}
	} else {
		x@file@nodatavalue <- value[[1]]
	}
	return(x)
}

NAvalue <- function(x) {
	if (inherits(x, 'RasterStack')) {
		sapply(x@layers, function(x) { x@file@nodatavalue })
	} else {
		return(x@file@nodatavalue)
	}
}

