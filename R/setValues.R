# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3




if (!isGeneric('setValues')) {
	setGeneric('setValues', function(object, values, layer=-1)
		standardGeneric('setValues')) 
	}	

	
setMethod('setValues', signature(object='RasterLayer'), 
function(object, values, layer=-1) {
  
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
		object@data@values <- values

		object@data@inmemory <- TRUE
		object@data@fromdisk <- FALSE

#		object@data@indices <- c(1, ncell(object))
		object <- setMinMax(object)
		return(object)
		
	} else if (length(values) == ncol(object)) {
		stop('setValues no longer supports setting rows of values')
#		rownr <- round(rownr)
#		if (rownr < 1 | rownr > nrow(object)) {
#			stop(paste("rownumber out of bounds:", rownr))
#		}
#		object@data@values <- values
#		object@data@content <- 'row' 
#		firstcell <- cellFromRowCol(object, rownr=rownr, colnr=1)
#		lastcell <- cellFromRowCol(object, rownr=rownr, colnr=ncol(object))
#		object@data@indices <- c(firstcell, lastcell)
#		return(object)
	} else {
		stop("length(values) is not equal to ncell(object), or to 1") 
	}
 }
)
	


setMethod('setValues', signature(object='RasterBrick'), 
  function(object, values, layer=-1) {
	if (!(is.vector(values) | is.matrix(values))) {
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
			object@data@nlayers <- ncol(values)
			object@data@inmemory <- TRUE
			
#			object@data@indices <- c(1, ncell(object))
			object@data@values <- values
			object <- setMinMax(object)
			
		} else if (nrow(values) == ncol(object)) {
			stop('setValues no longer supports setting rows of values')

#			if (!validRow(object, rownr)) {
#				stop(paste("rownumber out of bounds:", rownr))
#			}
#			if (object@data@nlayers != ncol(values)) {
#				if (rownr==1) {
#					object@data@nlayers <- ncol(values)
#				} else {
#					stop('ncol does not match nlayers' )
#				}
#			}	
#			object@data@content <- 'row'
#			firstcell <- cellFromRowCol(object, rownr=rownr, colnr=1)
#			lastcell <- cellFromRowCol(object, rownr=rownr, colnr=ncol(object))
#			object@data@indices <- c(firstcell, lastcell)				
#			object@data@values <- values
		} else {
			stop('either set all data or a single row')
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
			object@data@values[,layer] <- values
			object <- setMinMax(object)
			
		} else if (length(values) == ncol(object)) {
			stop('setValues no longer supports setting rows of values')

#			if (!validRow(object, rownr)) {
#				stop(paste("rownumber out of bounds:", rownr))
#			}
#			object@data@values <- values
#			object@data@content <- 'row' 
#			firstcell <- cellFromRowCol(object, rownr=rownr, colnr=1)
#			lastcell <- cellFromRowCol(object, rownr=rownr, colnr=ncol(object))
#			if (object@data@indices != c(firstcell, lastcell)) {
#				stop('setting values for the wrong row number')
#			}
		} else {
			stop("length(values) is not equal to ncell(object)") 
		}
	}
	return(object)
}
)
	

	