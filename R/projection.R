# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3



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
		x@crs = crs
	}
	return(x)
}


projection <- function(x, asText=TRUE) {
	if (extends(class(x), "BasicRaster")) { x <- x@crs 
	} else if (extends(class(x), "Spatial")) { x <- x@proj4string 
	} else if (class(x) == 'character') {return(x)
	} else if (class(x) != "CRS") { if (asText) { return("NA") } else { return(NA) }  }
	
	if (asText) {
		if (is.na(x@projargs)) { 
			return("NA") 
		} else {
			return(trim(x@projargs))
		}	
	} else {
		return(x)
	}
}


