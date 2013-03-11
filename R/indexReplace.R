# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 1.0
# Licence GPL v3


setReplaceMethod("[", c("RasterLayer", "RasterLayer", "missing"),
	function(x, i, j, value) {

		if (! hasValues(i) ) {
			i <- cellsFromExtent(x, i)
			
		} else if (compareRaster(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
			i <- as.logical( getValues(i) )
		
		} else {
			i <- cellsFromExtent(x, i)
		}		
	
		.replace(x, i, value=value, recycle=1) 
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


.replace <- function(x, i, value, recycle=1) {

	if (inherits(x, 'RasterStack')) {
		x <- brick(x, values=TRUE)
	}


	if ( is.logical(i) ) {
		i <- which(i)
	} else {
	#	if (! is.numeric(i)) { 
	#		i <- as.integer(i) 
	#	}
		i <- na.omit(i)
	}

	nl <- nlayers(x)
  # recycling
	if (nl > 1) {
		rec2 <- ceiling(nl / recycle)
		if (rec2 > 1) {
			add <- ncell(x)*recycle * (0:(rec2-1))
			i <- as.vector(t((matrix(rep(i, rec2), nrow=rec2, byrow=TRUE)) + add))
		}
	}
	i <- i[ i > 0 & i <= (ncell(x)*nl) ]
	
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


