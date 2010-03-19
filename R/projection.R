# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.9
# Licence GPL v3


'projection<-' <- function(x, value) {
	if (class(value)=="CRS") {
		x@crs <- value
	} else {	
		x@crs <- .newCRS(value)
	}	
	return(x)
}


projection <- function(x, asText=TRUE) {
	if (extends(class(x), "BasicRaster")) { x <- x@crs 
	} else if (extends(class(x), "Spatial")) { x <- x@proj4string 
	} else if (class(x) == 'character') {return(x)
	} else if (class(x) != "CRS") { stop(paste('cannot use this x of class', class(x))) }
	
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


