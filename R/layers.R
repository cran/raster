# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 1.0
# Licence GPL v3


if (!isGeneric("nlayers")) {
	setGeneric("nlayers", function(object)
		standardGeneric("nlayers"))
}	

setMethod('nlayers', signature(object='BasicRaster'), 
	function(object){
		return(0) 
    }
)

setMethod('nlayers', signature(object='Raster'), 
	function(object){
		return(1) 
    }
)

setMethod('nlayers', signature(object='RasterStack'), 
	function(object){
		as.integer( sum(unlist( sapply(object@layers, nlayers) ) ) )
    }
)

setMethod('nlayers', signature(object='RasterBrick'), 
	function(object){
		return(object@data@nlayers) 
    }
)

setMethod('nlayers', signature(object='Spatial'), 
	function(object){
		if (! is.null( attr(object, 'data') ) ) {
			return( dim(object@data)[2] ) 
		} else {
			return( 0 )
		}
    }
)

