# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("band")) {
	setGeneric("band", function(x, ...)
		standardGeneric("band"))
}	


setMethod('band', signature(x='RasterLayer'), 
function(x) {
	return(x@data@band)
}
)


nbands <- function(x) {
	cx = class(x)
	if (inherits(x, "RasterLayer") | inherits(x, "RasterBrick")) {
		return(x@file@nbands)
	} else {
		stop(paste("not implemented for", class(x), "objects"))
	}	
}


.bandOrder <- function(x) {
	if (class(x) == "RasterStack") {
		stop(paste("not implemented for RasterStack objects"))
	} else {
		return(paste(x@file@bandorder))
	}
}

