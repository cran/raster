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
			i[is.na(i)] <- FALSE
			
		} else {
			i <- cellsFromExtent(x, i)
		}		
	
		.replace(x, i, value=value) 
	}
)


setReplaceMethod("[", c("RasterLayer", "Extent", "missing"),
	function(x, i, j, value) {
	
		i <- cellsFromExtent(x, i)
		.replace(x, i, value=value)
	}
)

setReplaceMethod("[", c("RasterLayer", "Spatial", "missing"),
	function(x, i, j, value) {

		if (inherits(i, 'SpatialPolygons')) {
			v <- 1:length(i@polygons)
			v[] <- value
			return( .polygonsToRaster(i, x, field=v, fun='last', mask=FALSE, update=TRUE, updateValue="all", silent=TRUE) )
			
		} else if (inherits(i, 'SpatialLines')) {
			v <- 1:length(i@lines)
			v[] <- value
			return( .linesToRaster(i, x, field=v, fun='last', mask=FALSE, update=TRUE, updateValue="all", silent=TRUE) )
			
		} else { # if (inherits(i, 'SpatialPoints')) {
			i <- cellsFromXY(x, coordinates(i))
			return( .replace(x, i, value=value) )
		}
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

setReplaceMethod("[", c("RasterLayer", "numeric", "numeric"),
	function(x, i, j, value) {
		i <- cellFromRowColCombine(x, i, j)
		.replace(x, i, value)
	}
)	

setReplaceMethod("[", c("RasterLayer","missing", "numeric"),
	function(x, i, j, value) {
		j <- cellFromCol(x, j)
		.replace(x, j, value=value)
	}
)


setReplaceMethod("[", c("RasterLayer","numeric", "missing"),
	function(x, i, j, value) {
		theCall <- sys.call(-1)
		narg <- length(theCall)-length(match.call(call=sys.call(-1)))
		if (narg > 0) {
			i <- cellFromRow(x, i)
		}
		.replace(x, i=i, value=value)
	}
)




setReplaceMethod("[", c("RasterLayer", "logical", "missing"),
	function(x, i, j, value) {
		.replace(x, i, value)
	}
)	


.replace <- function(x, i, value) {

	if (! is.numeric(value) & !is.logical(value)) { 
		value <- as.numeric(value) 
	}
		
	if ( is.logical(i) ) {
		i[is.na(i)] <- FALSE
	} else {
		i <- na.omit(i)
		i <- subset(i, i >= 1 & i <= ncell(x))
	}
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			x <- try( readAll(x) )
		} else {
			x <- try (setValues(x, rep(NA, times=ncell(x))) )
		}
		if (class(x) == 'try-error') {
			stop('cannot do in-memory replace values on this raster (it is too large)')
		}
	}
		
		
	x@data@values[i] <- value
	x <- setMinMax(x)
	x <- .clearFile(x)
	return(x)
}


