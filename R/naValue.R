# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 1.0
# Licence GPL v3


'NAvalue<-' <- function(x, value) {
	if (inherits(x, 'RasterStack')) {
		for (i in 1:nlayers(x)) {
			x@layers[[i]]@file@nodatavalue <- value
		}
	} else {
		x@file@nodatavalue <- value
	}
	return(x)
}

'NAvalue' <- function(x, value) {
	if (inherits(x, 'RasterStack')) {
		sapply(x@layers, function(x) { x@file@nodatavalue })
	} else {
		return(x@file@nodatavalue)
	}
}

