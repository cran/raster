# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3


click <- function(object, n=1, id=FALSE, xy=FALSE, cell=FALSE, type="n", ...) {
	loc <- locator(n, type, ...)
	xyCoords <- cbind(x=loc$x, y=loc$y)
	if (missing(object)) { return(xyCoords) }

	if (id) {
		text(xyCoords, labels=1:n)
	}
	
	if (inherits(object, 'Spatial')) {
		res <- overlay(object, SpatialPoints(xyCoords))
		if (xy) {
			res <- cbind(xyCoords, res)
		}
		if (is.matrix(res)) {
			rownames(res) <- 1:n
		}
		return(res)
	} else if (!inherits(object, 'Raster')) {
		warning('object is not of a supported class')
		return(xyCoords)
	}
	
	cells <- cellFromXY(object, xyCoords)
	cells <- unique(na.omit(cells))
	if (length(cells) == 0 ) { stop('no valid cells selected') }
	xyCoords <- xyFromCell(object, cells)
	colnames(xyCoords) <- c('x', 'y')
	n <- nrow(xyCoords)

	value <- .cellValues(object, cells)

	if (nlayers(object) == 1)  {
		value <- matrix(value)
		colnames(value) <- layerNames(object)
	}
	
	if (cell) {
		value <- cbind(cells, value)
	}
	if (xy) { 
		value <- cbind(xyCoords, value)
	} 
	value
}

