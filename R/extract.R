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

	# focal values
	if ( ! is.null(dots$row) ) {
		warning("the 'row' argument is extract is depracated; use getValuesFocal instead")
		return( .focalValues(x, ...) )
	}

	# backwards in-compatability
	if (! is.null(dots$cells)) {
		stop("the 'cells' argument is depracated")
	}
	if (! is.null(dots$xy)) {
		stop("the 'xy' argument is depracated")
	}
	if (! is.null(dots$p)) {
		stop("the 'p' argument is depracated")
	}
	if (! is.null(dots$lns)) {
		stop("the 'lns' argument is depracated")
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
	px <- projection(x, asText=FALSE)
	comp <- .compareCRS(px, projection(y), unknown=TRUE)
	if (!comp) {
		.requireRgdal()
		warning('Transforming SpatialPoints to the CRS of the Raster')
		y <- spTransform(y, px)
	}
	.xyValues(x, coordinates(y), ...)
})




setMethod('extract', signature(x='Spatial', y='Raster'), 
function(x, y, ...){ 
# For backwards compatibility
	stop('the order of the first two arguments is reversed' )
})


setMethod('extract', signature(x='Raster', y='Extent'), 
 	function(x, y, fun, na.rm=FALSE, layer, nl, ...) {

		e <- intersect(extent(x), y)
		e <- alignExtent(e, x)
		
		r <- res(x)
		e@xmin <- e@xmin + 0.25 * r[1]
		e@xmax <- e@xmax - 0.25 * r[1]
		e@ymin <- e@ymin + 0.25 * r[2]
		e@ymax <- e@ymax - 0.25 * r[2]
	
		row <- rowFromY(x, e@ymax)
		lastrow <- rowFromY(x, e@ymin)
		nrows <- lastrow-row+1
		col <- colFromX(x, e@xmin)
		lastcol <- colFromX(x, e@xmax)
		ncols <- lastcol-col+1
		
		v <- getValuesBlock(x, row, nrows, col, ncols)  
		
		if (nlayers(x) > 1) {
			if (missing(layer)) {
				layer <- 1
			} else {
				layer <- max(min(nlayers(x), layer), 1)
			}
			if (missing(nl)) {
				nl <- nlayers(x) - layer + 1
			} else {
				nl <- max(min(nlayers(x)-layer+1, nl), 1)
			}
			lyrs <- layer:(layer+nl-1)
			v <- v[ , lyrs, drop=FALSE] 
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

