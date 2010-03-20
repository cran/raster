# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.9
# Licence GPL v3

#'nlayers<-' <- function(x, value) {
#	if (class(x) != 'RasterBrick') {
#		stop('only for RasterBrick objects')
#	}
#	x@data@nlayers <- value
#}

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
		return(length(object@layers)) 
    }
)

setMethod('nlayers', signature(object='RasterBrick'), 
	function(object){
		return(object@data@nlayers) 
    }
)

setMethod('nlayers', signature(object='Spatial'), 
	function(object){
		if ( class(object)=='SpatialPixelsDataFrame' |  class(object)=='SpatialGridDataFrame' ) { 
			return( dim(object@data)[2] ) 
		} else {
			return( 0 )
		}
    }
)

