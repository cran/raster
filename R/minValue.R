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
	function(x, layer=-1) {
		if ( x@data@haveminmax ) {
			return(x@data@min)
		} else {
			return(NA)
		}
	}
)

setMethod('minValue', signature(x='RasterBrick'), 
	function(x, layer=-1) {
		layer <- round(layer)
		if (layer < 1) { 
			return(x@data@min)
		} else {
			return(x@data@min[layer])
		}
	}
)


setMethod('minValue', signature(x='RasterStack'), 
	function(x, layer=-1) {
		layer <- round(layer)
		if (layer < 1) { 
			minv <- vector()
			for (i in 1:nlayers(x)) {
				minv[i] <- minValue(x@layers[[i]])
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
	function(x, layer=-1) {
		if ( x@data@haveminmax ) {
			return(x@data@max)
		} else {
			return(NA)
		}
	}
)

setMethod('maxValue', signature(x='RasterBrick'), 
	function(x, layer=-1) {
		layer <- round(layer)
		if (layer < 1) { 
			return(x@data@max)
		} else {
			return(x@data@max[layer])
		}
	}
)


setMethod('maxValue', signature(x='RasterStack'), 
	function(x, layer=-1) {
		layer <- round(layer)
		if (layer < 1) { 
			maxv <- vector()
			for (i in 1:nlayers(x)) {
				maxv[i] <- maxValue(x@layers[[i]])
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

		
