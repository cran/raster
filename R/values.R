# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3
	
#if (!isGeneric("values")) { 
#	setGeneric("values", function(x, ...)
#		standardGeneric("values"))
#}	


#setMethod('values', signature(x='RasterLayer'), 
#function(x, format='vector', names=FALSE, ...) {

#	if (! inMemory(x) ) {
#		stop("No data in memory. Use getValues()") 
#	}

#	warning('"values" is depracated; use getValues instead')
	
#	if (format=='matrix') { 
#		x = matrix(x@data@values, nrow=nrow(x), ncol=ncol(x), byrow=TRUE)
#		if (names) {
#			colnames(x) <- 1:ncol(x)
#			rownames(x) <- 1:nrow(x)
#		}
#		return(x)
#	} else {
#		return(x@data@values) 
#	}
#} )

