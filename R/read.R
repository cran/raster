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
		if (! object@data@fromdisk)  {
			stop('cannot read values; there is no file associated with this RasterLayer')
		}
		
		object@data@inmemory <- TRUE
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
		if (! object@data@fromdisk)  {
			stop('cannot read values; there is no file associated with this RasterLayer')
		}

		object@data@inmemory <- TRUE
		object@data@values <- .readRasterBrickValues(object, 1, object@nrows)
		return(object)
	}
)

