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


.replace <- function(x, i, value) {

	if (inherits(x, 'RasterStack')) {
		x <- brick(x, values=TRUE)
	}


	if (! is.numeric(value) & !is.logical(value)) { 
		value <- as.numeric(value) 
	}
		
	if ( is.logical(i) ) {
		i[is.na(i)] <- FALSE
	} else {
		i <- na.omit(i)
		i <- subset(i, i >= 1 & i <= (ncell(x)*nlayers(x)) )
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
	x <- raster:::.clearFile(x)
	return(x)
}


