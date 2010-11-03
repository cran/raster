# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 1.0
# Licence GPL v3




setReplaceMethod("[", "RasterLayer",
	function(x, i, j, value) {

	if (! missing(j) ) { 
		if (! is.numeric(j)) { 
			stop('the second argument must be numeric (or missing)') 
		}	
		if (! missing(i)) {
			if (! (is.numeric(i) | is.logical(i)) ) {
				stop('you cannot supply a second argument if the first is not numeric or logical') 		
			}
		}
	}

	if (! is.numeric(value) & !is.logical(value)) { value <- as.numeric(value) }
		
	if ( missing(i) ) {
		if (missing(j)) {
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
		} else {
			i <- cellFromCol(x, j)
		} 
	} else {
		if (inherits(i, 'Spatial')) {
			if (inherits(i, 'SpatialPolygons')) {
				v <- 1:length(i@polygons)
				v[] <- value
				return(polygonsToRaster(i, x, field=v, overlap='last', mask=FALSE, updateRaster=TRUE, updateValue="all", silent=TRUE) )
			}
			if (inherits(i, 'SpatialLines')) {
				v <- 1:length(i@lines)
				v[] <- value
				return(linesToRaster(i, x, field=v, overlap='last', mask=FALSE, updateRaster=TRUE, updateValue="all", silent=TRUE) )
			}
			stop('currently only implemented for SpatialLines* and SpatialPolygons*, not for other Spatial* objects')
		} else if (inherits(i, "RasterLayer")) {
			if (compare(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
				i <- as.logical( getValues(i) ) 
			} else {
				i <- extent(i)
			}
		} else if (inherits(i, "Extent")) {
			i <- cellsFromExtent(x, i)
		} else if (missing(j)) {
			theCall <- sys.call(-1)
			narg <- length(theCall)-length(match.call(call=sys.call(-1)))
			if (narg > 0) {
				i <- cellFromRow(x, i)
			}
		} else {
			i <- cellFromRowColCombine(x, i, j)
		}
	}

	i <- na.omit(i)
	if (! is.logical(i) ) {
		i <- subset(i, i >= 1 & i <= ncell(x))
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
		
		
	x@data@values[i] <- value
	x <- setMinMax(x)
	x <- .clearFile(x)
	return(x)
}
)

