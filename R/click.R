# Author: Robert J. Hijmans
# Date : January 2009 - December 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("click")) {
	setGeneric("click", function(x, ...)
		standardGeneric("click"))
}	

setMethod('click', signature(x='missing'), 
	function(x, n=1, type="n", ...) {
		loc <- locator(n, type, ...)
		cbind(x=loc$x, y=loc$y)
	}
)

	
setMethod('click', signature(x='SpatialGrid'), 
	function(x, n=1, id=FALSE, xy=FALSE, type="n", ...) {
		x <- brick(x)
		click(x, n=n, id=id, xy=xy, type=type, ...)
	}
)

setMethod('click', signature(x='SpatialPixels'), 
	function(x, n=1, id=FALSE, xy=FALSE, type="n", ...) {
		x <- brick(x)
		click(x, n=n, id=id, xy=xy, type=type, ...)
	}
)

setMethod('click', signature(x='Raster'), 
	function(x, n=1, id=FALSE, xy=FALSE, cell=FALSE, type="n", ...) {
	
	loc <- locator(n, type, ...)
	xyCoords <- cbind(x=loc$x, y=loc$y)

	if (id) {
		text(xyCoords, labels=1:n)
	}
	
	cells <- cellFromXY(x, xyCoords)
	cells <- unique(na.omit(cells))
	if (length(cells) == 0 ) { stop('no valid cells selected') }
	xyCoords <- xyFromCell(x, cells)
	colnames(xyCoords) <- c('x', 'y')
	n <- nrow(xyCoords)

	value <- .cellValues(x, cells)

	if (nlayers(x) == 1)  {
		value <- matrix(value)
		colnames(value) <- layerNames(x)
	}
	
	if (cell) {
		value <- cbind(cells, value)
	}
	if (xy) { 
		value <- cbind(xyCoords, value)
	} 
	value
}
)

