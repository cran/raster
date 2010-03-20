# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



click <- function(object, n=1, id=FALSE, xy=FALSE, type="n", ...) {
	loc <- locator(n, type, ...)
	xyCoords <- cbind(loc$x, loc$y)
	if (missing(object)) { return(xyCoords) }
	cells <- cellFromXY(object, xyCoords)
	cells <- unique(na.omit(cells))
	if (length(cells) == 0 ) { stop('no valid cells selected') }
	xyCoords <- xyFromCell(object, cells)
	colnames(xyCoords) <- c('x', 'y')
	n <- nrow(xyCoords)
	if (id) {
		for (i in 1:n) {
			text(xyCoords[i,1], xyCoords[i,2], i)
		}
	}

	value <- cellValues(object, cells)

	if (class(object) == 'RasterStack' | class(object) == 'RasterBrick') {
		value <- t(matrix(value, nrow=n))
		rownames(value) <- layerNames(object)
	} else {
		value <- t(matrix(value))
		if (layerNames(object) == "") {
			rownames(value) <- 'value'
		} else {
			rownames(value) <- layerNames(object)
		}
	}
	
	if (xy) { 
		value <- rbind(t(xyCoords), value)
	} 
	colnames(value) <- 1:n
	return(t(value))
	
}
