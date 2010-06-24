# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("readAll")) {
	setGeneric("readAll", function(object)
		standardGeneric("readAll"))
}

	
setMethod('readAll', signature(object='RasterLayer'), 
	function(object){ 
		object@data@content <- 'all' 
		object@data@indices <- c(1, ncell(object))
		object@data@values <- .readRasterLayerValues(object, 1, object@nrows)
		return(object)
	}
)


setMethod('readAll', signature(object='RasterStack'), 
	function(object){ 
		for (i in seq(nlayers(object))) {
			object@layers[[i]] <- readAll(object@layers[[i]])
		}
		object
	}
)


setMethod('readAll', signature(object='RasterBrick'), 
	function(object){ 
		object@data@content <- 'all' 
		object@data@indices <- c(1, ncell(object))
		object@data@values <- .readRasterBrickValues(object, 1, object@nrows)
		return(object)
	}
)

