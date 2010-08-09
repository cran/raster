# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 1.0
# Licence GPL v3



setReplaceMethod("[", c("RasterLayer", "ANY", "missing"),
	function(x, i, j, value) {
		
		if (!is.numeric(value)) value <- as.numeric(value)
		
		if  (missing(i)) {
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


		if (! inMemory(x) ) {
			if ( fromDisk(x) ) {
				x <- try( readAll(x) )
			} else {
				x <- try (setValues(x, rep(NA, times=ncell(x))) )
			}
			if (class(x) == 'try-error') {
				stop('cannot replace values on this raster (it is too large')
			}
		}
		
		
		if (class(i) == "RasterLayer") {
			i <- as.logical( getValues(i) ) 
		}
		if (!is.logical(i)) {
			i <- subset(i, i >= 1 & i <= ncell(x))
		}

		x@data@values[i] <- value
		x@data@fromdisk <- FALSE
		
		filename(x) <- ""
		x <- setMinMax(x)
		return(x)
	}
)

