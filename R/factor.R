# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : February 2010
# Version 0.9
# Licence GPL v3


	
if (!isGeneric("is.factor")) {
	setGeneric("is.factor", function(x)
		standardGeneric("is.factor"))
}	

setMethod('is.factor', signature(x='Raster'), 
	function(x) {
		return(x@data@isfactor)
	}
)

setMethod('is.factor', signature(x='RasterStack'), 
	function(x) {
		sapply(x@layers, function(x) x@data@isfactor)
	}
)


if (!isGeneric("labels")) {
	setGeneric("labels", function(object, ...)
		standardGeneric("labels"))
}	

setMethod('labels', signature(object='Raster'), 
	function(object, ...) {
		return(object@data@attributes)
	}
)

setMethod('labels', signature(object='RasterStack'), 
	function(object, ...) {
		sapply(object@layers, function(x) x@data@attributes) 
	}
)


if (!isGeneric("labels<-")) {
	setGeneric("labels<-", function(object, value)
		standardGeneric("labels<-"))
}	


setMethod('labels<-', signature(object='RasterLayer', value='list'), 
	function(object, value) {
		if (length(value) != 1) {
			stop('length(value) != 1')
		}
		object@data@attributes <- value
		return(object)
	}
)

setMethod('labels<-', signature(object='RasterBrick', value='list'), 
	function(object, value) {
		if (length(value) != nlayers(object)) {
			stop('length(value) != nlayers(object)')
		}
		object@data@attributes <- value
		return(object)
	}
)



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
	function(x, value=NULL, ...) {
		x@data@isfactor <- TRUE
		if (is.null(value) ) {
			#x <- round(x) #this makes slot isfactor FALSE again
			x@data@attributes <- list(data.frame(VALUE=unique(x)))
		} else {
			x@data@attributes <- value
		}	
		return(x)
	}
)

.asFactor <- function(x, value){

		return(x)
}		

setMethod('asFactor', signature(x='RasterBrick'), 
	function(x, value=NULL, ...) {
		x@data@isfactor <- TRUE
		if (is.null(value) ) {
			#x <- round(x) #this makes slot isfactor FALSE again
			x@data@attributes <- list(data.frame(VALUE=unique(x)))
		} else {
			x@data@attributes <- value
		}			
		return(x)
	}
)

