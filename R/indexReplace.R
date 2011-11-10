# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 1.0
# Licence GPL v3


setReplaceMethod("[", c("RasterLayer", "RasterLayer", "missing"),
	function(x, i, j, value) {

		if (! hasValues(i) ) {
			i <- cellsFromExtent(x, i)
			
		} else if (compare(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
			i <- as.logical( getValues(i) )
		
		} else {
			i <- cellsFromExtent(x, i)
		}		
	
		.replace(x, i, value=value) 
	}
)



setReplaceMethod("[", c("RasterLayer","missing","missing"),
	function(x, i, j, value) {
	
		if (length(value) == ncell(x)) {
			x <- try( setValues(x, value))
		} else if (length(value) == 1) {
			x <- try( setValues(x, rep(value, times=ncell(x))) )
		} else {
			v <- try( vector(length=ncell(x)) )
			if (class(x) != 'try-error') {
				v[] <- value
				x <- try( setValues(x, v) )
			}
		}
		if (class(x) == 'try-error') {
			stop('cannot replace values on this raster (it is too large')
		}
		return(x)
	
	}
)


.replace <- function(x, i, value, allLayers=TRUE) {

	if (inherits(x, 'RasterStack')) {
		x <- brick(x, values=TRUE)
	}


	if ( is.logical(i) ) {
		i <- which(i)
	} else {
		if (! is.numeric(value)) { 
			value <- as.numeric(value) 
		}
		i <- na.omit(i)
	}

	if (allLayers) {
		i <- subset(i, i >= 1 & i <= ncell(x) )
		nl <- nlayers(x)
		if (nl > 1) {
			add <- ncell(x) * 0:(nl-1)
			i <- as.vector(t((matrix(rep(i, nl), nr=nl, byrow=TRUE)) + add))
		}
	}
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			x <- try( readAll(x) )
		} else {
			x <- try (setValues(x, rep(NA, times=ncell(x))) )
		}
		if (class(x) == 'try-error') {
			stop('cannot do in-memory replace values on this raster (it is too large).\nYou can use the "calc" function instead.')
		}
	}
		
		
	x@data@values[i] <- value
	x <- setMinMax(x)
	x <- .clearFile(x)
	return(x)
}


