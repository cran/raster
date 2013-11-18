# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3



if (!isGeneric("crs")) {
	setGeneric("crs", function(x, ...)
		standardGeneric("crs"))
}	

setMethod("crs", signature('ANY'), 
	function(x, asText=TRUE, ...) {
		projection(x, asText=asText)
	}
)


'crs<-' <- function(x, value) {
	projection(x) <- value
	x
}

'projection<-' <- function(x, value) {

	if (class(value)=="CRS") {
		crs <- value
	} else {	
		crs <- .newCRS(value)
	}	
	
	if (inherits(x, 'RasterStack')) {
		if (nlayers(x) > 0) {
			for (i in 1:nlayers(x)) {
				x@layers[[i]]@crs <- crs
			}
		}
	} 
	
	if (inherits(x, 'Spatial')) {
		x@proj4string <- crs
	} else {
		x@crs <- crs
	}
	return(x)
	
}



projection <- function(x, asText=TRUE) {

	if (extends(class(x), "BasicRaster")) { 
		x <- x@crs 
	} else if (extends(class(x), "Spatial")) { 
		x <- x@proj4string
	} else if (class(x) == 'character') { 
		if (asText) {
			return(x)
		} else {
			return( CRS(x) )
		}
	} else if (class(x) != "CRS") { 
		if (asText) { 
			return(NA)
		} else { 
			return(NA) 
		}  
	}
	
	if (asText) {
		if (is.na(x@projargs)) { 
			return(NA) 
		} else {
			return(trim(x@projargs))
		}	
	} else {
		return(x)
	}
}


setMethod("proj4string", signature('Raster'), 
# redundant, for compatibility with sp
	function(obj) {
		projection(obj)
	}
)


setMethod("proj4string<-", signature('Raster'), 
# redundant, for compatibility with sp
	function(obj, value) {
		projection(obj) <- value
		obj
	}
)

