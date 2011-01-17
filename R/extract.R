# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2010
# Version 1.0
# Licence GPL v3


if (!isGeneric("extract")) {
	setGeneric("extract", function(x, y, ...)
		standardGeneric("extract"))
}	


setMethod('extract', signature(x='Raster', y='missing'), 
function(x, y, ...){ 

	dots <- list(...)
	# backwards compatability
	if (! is.null(dots$cells)) {
		warning("the 'cells' argument is depracated")
		return( .cellValues(x, ...) )
	}
	if (! is.null(dots$xy)) {
		warning("the 'xy' argument is depracated")
		if (! is.matrix(dots$xy) ) {
			stop('xy must be a matrix')
		}
		return( .xyValues(x, ...) )
	}
	if (! is.null(dots$p)) {
		warning("the 'p' argument is depracated")
		return( .polygonValues(x, ...) )
	}
	if (! is.null(dots$lns)) {
		warning("the 'lns' argument is depracated")
		return( .lineValues(x, ...) )
	}
	
	# focal values
	if ( ! is.null(dots$row) ) {
		return( .focalValues(x, ...) )
	}
	stop('I do not understand what you want me to do')
	
	# return(getValues(x, ...)) ???
})



setMethod('extract', signature(x='Raster', y='vector'), 
function(x, y, ...){ 
	y <- round(y)
	if (length(y) == 2) {
		warning("returning values at CELL NUMBERS (not coordinates) : ", y[1], " and ", y[2])
	}
	return( .cellValues(x, y, ...) )
})


setMethod('extract', signature(x='Raster', y='matrix'), 
function(x, y, ...){ 
	return( .xyValues(x, y, ...) )
})



setMethod('extract', signature(x='Raster', y='data.frame'), 
function(x, y, ...){ 
	return( .xyValues(x, as.matrix(y), ...))
})


setMethod('extract', signature(x='Raster', y='SpatialPoints'), 
function(x, y, ...){ 
	return( .xyValues(x, coordinates(y), ...))
})


setMethod('extract', signature(x='Raster', y='SpatialLines'), 
function(x, y, ...){ 
	.lineValues(x, y, ...)
})


setMethod('extract', signature(x='Raster', y='SpatialPolygons'), 
function(x, y, ...){ 
	.polygonValues(x, y, ...)
})

setMethod('extract', signature(x='Spatial', y='Raster'), 
function(x, y, ...){ 
# For backwards compatibility
	stop('the order of the first two arguments is reversed' )
})


setMethod('extract', signature(x='Raster', y='Extent'), 
 	function(x, y, fun, na.rm=FALSE, lyrs, ...) {

		e <- intersectExtent(x, y)
		e <- alignExtent(e, x)
		
		row = rowFromY(x, e@ymax)
		lastrow = rowFromY(x, e@ymin)
		nrows = lastrow-row+1
		col = colFromX(x, e@xmin)
		lastcol = colFromX(x, e@xmax)
		ncols = lastcol-col+1
		
		v <- getValuesBlock(x, row, nrows, col, ncols)  
		
		if (! missing(lyrs) ) {
			nl <- nlayers(x)
			lyrs <- lyrs[lyrs %in% 1:nl]
			if (length(lyrs) > 1) {
				v <- v[ , lyrs, drop=FALSE] 
			}
		}
		
		if (! missing(fun)) {
			if (is.matrix(v)) {
				ln <- colnames(v)
				v <- apply(v, 2, FUN=fun, na.rm=na.rm)
				names(v) <- ln
			} else {
				v <- fun(v, na.rm=na.rm)
			}
		}
		return(v)
	}
)

