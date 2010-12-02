# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3


	
rowFromCell <- function(object, cell) {
	object <- raster(object)
	cell <- round(cell)
	cell[cell < 1 | cell > ncell(object)] <- NA
	rownr <- as.integer(trunc((cell-1)/ncol(object)) + 1)
    return(rownr)
}


cellFromRow <- function(object, rownr) {
	object <- raster(object)
	rownr <- round(rownr)
	if (length(rownr)==1) {
		return(cellFromRowCol(object, rownr, 1):cellFromRowCol(object, rownr, object@ncols))
	}
	cols <- rep(1:ncol(object), times=length(rownr))
	rows <- rep(rownr, each=ncol(object))	
	return(cellFromRowCol(object, rows, cols))
}

cellFromCol <- function(object, colnr) {
	object <- raster(object)
	colnr <- round(colnr)
	rows <- rep(1:nrow(object), times=length(colnr))
	cols <- rep(colnr, each=nrow(object))
	return(cellFromRowCol(object, rows, cols))
}


.OLD_cellFromRowColCombine <- function(object, rownr, colnr) {
	object <- raster(object)
	rc <- expand.grid(rownr, colnr)
	return( cellFromRowCol(object, rc[,1], rc[,2]))
}


cellFromRowColCombine <- function(object, rownr, colnr) {
	object <- raster(object)
	rownr[rownr < 1 | rownr > object@nrows] = NA
	colnr[colnr < 1 | colnr > object@ncols] = NA
	cols = rep(colnr, times=length(rownr))
	dim(cols) = c(length(colnr), length(rownr))
	cols = t(cols)
	rownr = (rownr-1) * object@ncols
	cols = cols + rownr
	return(as.vector(t(cols)))
}


colFromCell <- function(object, cell) {
	object <- raster(object)
	cell <- round(cell)
	cell[cell < 1 | cell > ncell(object)] <- NA	
	rownr <- as.integer(trunc((cell-1)/ncol(object)) + 1)
	colnr <- as.integer(cell - ((rownr-1) * ncol(object)))
    return(colnr)
}

cellFromRowCol <- function(object, rownr, colnr) {
	object <- raster(object)
	rownr <- round(rownr)
	colnr <- round(colnr)
	rownr[rownr < 1 | rownr > nrow(object)] <- NA
	colnr[colnr < 1 | colnr > ncol(object)] <- NA	
	# recycle if length(rownr) != lenght(colnr)
	x <- cbind(rownr, colnr)
	return((x[,1]-1) * ncol(object) + x[,2])
}

