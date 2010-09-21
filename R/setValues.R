# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric('setValues')) {
	setGeneric('setValues', function(object, values, layer=-1)
		standardGeneric('setValues')) 
	}	

	
setMethod('setValues', signature(object='RasterLayer'), 
function(object, values) {

	if (is.matrix(values)) { 
		if (ncol(values) == object@ncols & nrow(values) == object@nrows) {
			values <- as.vector(t(values)) 
		} else if (ncol(values)==1 | nrow(values)==1) {
			values <- as.vector(values)
		} else {
			stop('cannot use a matrix with these dimensions')
		}
	}
  
	if (!is.vector(values)) {stop('values must be a vector')}
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	}
	

	if (length(values) == 1) {	
		values <- rep(values, ncell(object))
	}

	if (length(values) == ncell(object)) { 
		object@data@inmemory <- TRUE
		object@data@fromdisk <- FALSE
		object@file@name = ""

		object@data@values <- values
		object <- setMinMax(object)
		return(object)
		
	} else if (length(values) == ncol(object)) {
		stop('setValues no longer supports setting rows of values')

	} else {
		stop("length(values) is not equal to ncell(object), or to 1") 
	}
 }
)
	


setMethod('setValues', signature(object='RasterBrick'), 
  function(object, values, layer=-1) {
  
	if ( ! (is.vector(values) | is.matrix(values)) ) {
		stop('values must be a vector or a matrix')
	}
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	
	}

#	rownr <- round(rownr)

	if (layer < 1) {
		if (!is.matrix(values)) {
			values <- matrix(values)
		}
		if (nrow(values) == ncell(object)) {

			object@file@name = ""
			object@data@inmemory <- TRUE
			object@data@fromdisk <- FALSE
			object@data@nlayers <- ncol(values)
			object@data@values <- values
			object <- setMinMax(object)
			 
		} else {
			stop('data size is not correct')
		}
	} else {
		if (nlayers(object)==0) {object@data@nlayers <- 1 }
		layer <- round(layer)
		if (layer > nlayers(object)) {stop('layer number too high')}
		
		if (length(values) == ncell(object)) { 
			if ( ! inMemory(object) ) { 
				atry <- try(object <- readAll(object), silent=T)
				if (class(atry) == "try-error") {
					stop("you can only setValues for a single layer if all values are in memory. But values could not be loaded")				
				}
			}
			object@file@name = ""
			object@data@inmemory <- TRUE
			object@data@fromdisk <- FALSE
			object@data@values[,layer] <- values
			object <- setMinMax(object)
			
		} else {
			stop("length(values) is not equal to ncell(object)") 
		}
	}
	return(object)
}
)
	

	