# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2010
# Version 1.0
# Licence GPL v3


if (!isGeneric("extract")) {
	setGeneric("extract", function(x, y, ...)
		standardGeneric("extract"))
}	



setMethod('extract', signature(x='Raster', y='vector'), 
function(x, y, ...){ 
	y <- round(y)
	if (length(y) == 2) {
		cat("note: returning values at CELL NUMBERS (not coordinates) : ", y[1], " and ", y[2], "\n")
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



setMethod('extract', signature(x='Raster', y='Extent'), 
 	function(x, y, cellnumbers=FALSE, fun=NULL, na.rm=FALSE, layer=1, nl, df=FALSE, ...) {

		e <- intersect(extent(x), y)
		e <- alignExtent(e, x)

		if (!is.null(fun)) {
			cellnumbers <- FALSE
		} else if (cellnumbers) {
			cell <- cellsFromExtent(x, e)
			value <- extract(x, cell, layer=layer, nl=nl, df=df)
			if (df) {
				value <- data.frame(cell=cell, value)
			} else {
				value <- cbind(cell=cell, value)
			}
			return(value)
		}
		
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
		} else {
			lyrs <- 1
		}
		
		if (! is.null(fun)) {
			if (is.matrix(v)) {
				ln <- colnames(v)
				v <- apply(v, 2, FUN=fun, na.rm=na.rm)
				names(v) <- ln
			} else {
				v <- fun(v, na.rm=na.rm)
			}
		}

		if (df) {
			v <- data.frame(v)
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v, lyrs))
			} else {
				v <- .insertFacts(x, v, lyrs)
			}
		}
		return(v)
	}
)



