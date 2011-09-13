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
		x@file@name <- ""
		x@file@driver <- ""
		x@data@values <- values
		x <- setMinMax(x)
		return(x)
		
	} else {
		stop("length(values) is not equal to ncell(x), or to 1") 
	}
 }
)
	

setMethod('setValues', signature(x='RasterStack'), 
	function(x, values, layer=-1) {
		b <- brick(x, values=TRUE)
		return(setValues(b, values, layer))
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
			#if (dm[1] == dm[2]) { warning('assuming values should be transposed') }
			transpose <- TRUE
		} else if (dmb[1] != dm[1] | dmb[2] != dm[2]) {
			stop('dimnesions of array do not match the RasterBrick')
		}
# speed imrovements suggested by Justin  McGrath
# http://pastebin.com/uuLvsrYc
		if (!transpose) {
			values <- aperm(values, c(2, 1, 3))
		}
		attributes(values) <- NULL
		dim(values) <- c(dm[1] * dm[2], dm[3])
###		
		
	} else if ( ! (is.vector(values) | is.matrix(values)) ) {
		stop('values must be a vector or a matrix')
	}
	
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	
	}

#	rownr <- round(rownr)

	if (layer < 1) {
		if (!is.matrix(values)) {
			values <- matrix(values, nrow=ncell(x), ncol=nlayers(x))
		}
		if (nrow(values) == ncell(x)) {

			x@file@name <- ""
			x@file@driver <- ""
			x@data@inmemory <- TRUE
			x@data@fromdisk <- FALSE
			x@data@nlayers <- ncol(values)
			cn <- colnames(values)
			if (!is.null(cn)) {
				x@layernames <- cn
			}
			x@data@values <- values
			x <- setMinMax(x)
			 
		} else {
			stop('data size is not correct')
		}
		
	} else {
	
		if (nlayers(x)==0) { 
			x@data@nlayers <- 1
		}
		layer <- round(layer)
		if (layer > nlayers(x)) { 
			stop('layer number too high') 
		}
		
		if (length(values) == ncell(x)) { 
			if ( ! inMemory(x) ) { 
				atry <- try(x <- readAll(x), silent=T)
				if (class(atry) == "try-error") {
					stop("you can only setValues for a single layer if all values are in memory. But values could not be loaded")				
				}
			}
			x@file@name <- ""
			x@file@driver <- ""
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

