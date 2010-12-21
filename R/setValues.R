# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric('setValues')) {
	setGeneric('setValues', function(x, values, layer=-1)
		standardGeneric('setValues')) 
	}	

	
setMethod('setValues', signature(x='RasterLayer'), 
function(x, values) {

	if (is.matrix(values)) { 
		if (ncol(values) == x@ncols & nrow(values) == x@nrows) {
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
		values <- rep(values, ncell(x))
	}

	if (length(values) == ncell(x)) { 
		x@data@inmemory <- TRUE
		x@data@fromdisk <- FALSE
		x@file@name = ""

		x@data@values <- values
		x <- setMinMax(x)
		return(x)
		
	} else if (length(values) == ncol(x)) {
		stop('setValues no longer supports setting rows of values')

	} else {
		stop("length(values) is not equal to ncell(x), or to 1") 
	}
 }
)
	

setMethod('setValues', signature(x='RasterStack'), 
	function(x, values, layer=-1) {
		layer <- layer[1]

		if (layer<1) {
			b <- brick(x, values=FALSE)
			return(setValues(b, values))
		} else {
			b <- brick(x, values=TRUE)
			return(setValues(b, values, layer))
		}
	}	
 )
	
	

	
setMethod('setValues', signature(x='RasterBrick'), 
	function(x, values, layer=-1) {
	
	layer <- layer[1]
	
	if (is.array(values) & !is.matrix(values)) {	
		dm <- dim(values)
		if (length(dm) != 3) {
			stop('array has wrong number of dimensions (needs to be 3)')
		}
		dmb <- dim(x)
		transpose <- FALSE
		if (dmb[1] == dm[2] & dmb[2] == dm[1]) {
			transpose <- TRUE
		} else if (dmb[1] != dm[1] | dmb[2] != dm[2]) {
			stop('dimnesions of array do not match the RasterBrick')
		}
		
		values <- matrix(as.vector(values), ncol=dm[3])
		if (transpose) {
			for (i in 1:ncol(values)) {
				values[,i] <- as.vector(matrix(values[,i], ncol=dm[2]))
			}
		} else {
			for (i in 1:ncol(values)) {
				values[,i] <- as.vector(t(matrix(values[,i], ncol=dm[2])))
			}
		}
		
	} else if ( ! (is.vector(values) | is.matrix(values)) ) {
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
		if (nrow(values) == ncell(x)) {

			x@file@name = ""
			x@data@inmemory <- TRUE
			x@data@fromdisk <- FALSE
			x@data@nlayers <- ncol(values)
			x@data@values <- values
			x <- setMinMax(x)
			 
		} else {
			stop('data size is not correct')
		}
	} else {
		if (nlayers(x)==0) { x@data@nlayers <- 1 }
		layer <- round(layer)
		if (layer > nlayers(x)) { stop('layer number too high') }
		
		if (length(values) == ncell(x)) { 
			if ( ! inMemory(x) ) { 
				atry <- try(x <- readAll(x), silent=T)
				if (class(atry) == "try-error") {
					stop("you can only setValues for a single layer if all values are in memory. But values could not be loaded")				
				}
			}
			x@file@name = ""
			x@data@inmemory <- TRUE
			x@data@fromdisk <- FALSE
			x@data@values[,layer] <- values
			x <- setMinMax(x)
			
		} else {
			stop("length(values) is not equal to ncell(x)") 
		}
	}
	return(x)
}
)

