# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3

setMethod("[[", "Raster",
function(x,i,j,...,drop=TRUE) {
	stop('this method is not implemented for Raster objects, use single brackets instead')
})


setMethod("[", c("Raster", "Spatial", "missing"),
function(x, i, j, ... ,drop=TRUE) {
	if (inherits(i, 'SpatialGrid') | inherits(i, 'SpatialPixels')) {
		i <-  as(i, 'SpatialPoints')
	}
	extract(x, i, ...)
})


setMethod("[", "Raster",
function(x,i,j,drop=TRUE) {
	
	if (! hasValues(x) ) {
		stop('no data associated with this RasterLayer object')
	}

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
	
	if (missing(i)) {
		if (missing(j)) {
			return(getValues(x))
		} else {
			i <- cellFromCol(x, j)
		}
	} else {
		if (inherits(i, "RasterLayer")) {
			i <- 1:ncell(i)[ as.logical( getValues(i) ) ]
		} else if (inherits(i, "Extent")) {
			return( extract(x, i) )
		} else {
			if (missing(j)) {
				theCall <- sys.call(-1)
				narg <- length(theCall) - length(match.call(call=sys.call(-1)))
				if (narg > 0) {
					i <- cellFromRow(x, i)
				}
			} else {
				i <- cellFromRowColCombine(x, i, j)
			}
		}
	}
	nacount <- sum(is.na(i))
	if (nacount > 0) {
		warning('some indices are invalid (NA returned)')
	}	
	return( .cellValues(x, i) )
}
)
