# raster package
# Authors: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3



if (!isGeneric("minValue")) {
	setGeneric("minValue", function(x, ...)
		standardGeneric("minValue"))
}	

setMethod('minValue', signature(x='RasterLayer'), 
	function(x, layer=-1, warn=TRUE) {
		if ( x@data@haveminmax ) {
			if (! inMemory(x) ) {
				return(x@data@min * x@data@gain + x@data@offset)
			} else {
				return(x@data@min)
			}
		} else {
			if (warn) warning('min value not known, use setMinMax')
			return(NA)
		}
	}
)


setMethod('minValue', signature(x='RasterBrick'), 
	function(x, layer=-1) {
		layer <- round(layer)
		if (layer < 1) { 
			if ( x@data@haveminmax ) {
				if (! inMemory(x) ) {
					return(x@data@min * x@data@gain + x@data@offset)
				} else {
					return(x@data@min)				
				}
			} else {
				warning('min value not known, use setMinMax')
				return(rep(NA, nlayers(x)))
			}
		} else {
			if ( x@data@haveminmax ) {
				return(x@data@min[layer] * x@data@gain + x@data@offset)
			} else {
				warning('min value not known, use setMinMax')
				return(NA)
			}
		}
	}
)


setMethod('minValue', signature(x='RasterStack'), 
	function(x, layer=-1) {
		layer <- round(layer)
		if (layer < 1) { 
			minv <- vector()
			for (i in 1:nlayers(x)) {
				minv[i] <- minValue(x@layers[[i]], warn=FALSE)
			}		
		} else {
			if (layer > 0 & layer <= nlayers(x)) {
				minv <- minValue(x@layers[[layer]])
			} else {
				stop('incorrect layer number')
			}
		}
		return(minv)
	}
)




if (!isGeneric("maxValue")) {
	setGeneric("maxValue", function(x, ...)
		standardGeneric("maxValue"))
}	

setMethod('maxValue', signature(x='RasterLayer'), 
	function(x, layer=-1, warn=TRUE) {
		if ( x@data@haveminmax ) {
			return(x@data@max * x@data@gain + x@data@offset)
		} else {
			if (warn) warning('max value not known, use setMinMax')
			return(NA)
		}
	}
)

setMethod('maxValue', signature(x='RasterBrick'), 
	function(x, layer=-1) {
		layer <- round(layer)
		if (layer < 1) { 
			if ( x@data@haveminmax ) {
				return(x@data@max * x@data@gain + x@data@offset)
			} else {
				warning('max value not known, use setMinMax')
				return(rep(NA, nlayers(x)))
			}
		} else {
			if ( x@data@haveminmax ) {
				return(x@data@max[layer] * x@data@gain + x@data@offset)
			} else {
				warning('max value not known, use setMinMax')
				return(NA)
			}
		}
	}
)


setMethod('maxValue', signature(x='RasterStack'), 
	function(x, layer=-1) {
		layer <- round(layer)
		if (layer < 1) { 
			maxv <- vector()
			for (i in 1:nlayers(x)) {
				maxv[i] <- maxValue(x@layers[[i]], warn=FALSE)
			}		
		} else {
			if (layer > 0 & layer <= nlayers(x)) {
				maxv <- maxValue(x@layers[[layer]])
			} else {
				stop('incorrect layer number')
			}
		}
		return(maxv)
	}
)

		
