# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.clearRaster <- function(object) {
	object@data@inmemory <- FALSE
	
	
#	object@data@indices = vector(mode='numeric')
	object@data@values <- vector()
	if ( !  fromDisk(object) ) {
		object@data@min <- Inf
		object@data@max <- -Inf	
		object@data@haveminmax <- FALSE
	}	
	return(object)
}


clearValues <- function(object) {
	if (class(object) == "BasicRaster" ) {
		return(object)
	} else if (class(object) == "RasterLayer" ) {
		object <- .clearRaster(object)
	} else if (class(object) == "RasterStack" ) {
		for (i in seq(along=nlayers(object))) {
			if (fromDisk(object@layers[[i]])) {
				object@layers[[i]] <- .clearRaster(object@layers[[i]])
			}
		}
	} else if (class(object) == 'RasterBrick') {
		object@data@values <- matrix(NA,0,0)
		object@data@inmemory <- FALSE
		
#		object@data@indices = c(0,0)
		if ( !  fromDisk(object) ) {
			object@data@min <- rep(Inf, nlayers(object))
			object@data@max <- rep(-Inf, nlayers(object))
			object@data@haveminmax <- FALSE
		}
	} 
	return(object)
}



