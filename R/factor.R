# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : February 2010
# Version 0.9
# Licence GPL v3


	
if (!isGeneric("is.factor")) {
	setGeneric("is.factor", function(x)
		standardGeneric("is.factor"))
}	

setMethod('is.factor', signature(x='RasterLayer'), 
	function(x) {
		return(x@data@isfactor)
	}
)

setMethod('is.factor', signature(x='RasterStack'), 
	function(x) {
		sapply(x@layers, function(x) x@data@isfactor)
	}
)


if (!isGeneric("levels")) {
	setGeneric("levels", function(x)
		standardGeneric("levels"))
}	

setMethod('levels', signature(x='RasterLayer'), 
	function(x) {
		return(x@data@levels)
	}
)

if (!isGeneric("labels")) {
	setGeneric("labels", function(object, ...)
		standardGeneric("labels"))
}	

setMethod('labels', signature(object='RasterLayer'), 
	function(object, ...) {
		return(object@data@labels)
	}
)


'labels<-' <- function(object, value) {
	if (is.factor(object)) {
		asFactor(object, levels(object), value)
	} else {
		stop('x is not a factor')
	}
}


if (!isGeneric("asFactor")) {
	setGeneric("asFactor", function(x, ...)
		standardGeneric("asFactor"))
}

setMethod('asFactor', signature(x='ANY'), 
	function(x, ...) {
		return(factor(x, ...))
	}
)

setMethod('asFactor', signature(x='RasterLayer'), 
	function(x, levels=NULL, labels=NULL, ...) {
		x@data@isfactor = TRUE
		if ( is.null(levels) ) {
			x@data@levels = unique(round(x))
		} else {
			x@data@levels = levels
		}
		if ( is.null(labels) ) {
			x@data@labels = as.character(x@data@levels)
		} else {
			if (length(labels) != length(x@data@levels)) {
				stop('number of labels does not match number of levels')
			}
			x@data@labels = as.character(labels)
		}
		return(x)
	}
)


setMethod('asFactor', signature(x='RasterStack'), 
	function(x, v, ...) {
		if (missing(v)) v = -1
		if (v < 1 | v < nlayers(x)) { stop('provide a valid argument "v" to indicate the layer') }
		x@layers[[v]] = asFactor(x@layers[[v]], ...)
		return(x)
	}
)

#if (!isGeneric("as.numeric")) {
#	setGeneric("as.numeric", function(x, ...)
#		standardGeneric("as.numeric"))
#}

setMethod('as.numeric', signature(x='RasterLayer'), 
	function(x, ...) {
		x@data@isfactor = FALSE
		x@data@levels = vector(mode='numeric')
		x@data@labels = vector(mode='character')
		return(x)
	}
)

setMethod('as.numeric', signature(x='RasterStack'), 
	function(x, v, ...) {
		if (missing(v)) v = -1
		if (v < 1 | v < nlayers(x)) { stop('provide a valid argument "v" to indicate the layer') }
		x@layers[[v]] <- as.numeric(x@layers[[v]])
		return(x)
	}
)
