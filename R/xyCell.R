# Author: Robert J. Hijmans
# Date :  October 2008
# Version 1.0
# Licence GPL v3


yFromRow <- function(object, rownr) {
	if (rotated(object)) {
		stop('this function is not supported for rotated rasters')
	}
	rownr <- round(rownr)
	rownr[rownr < 1 | rownr > object@nrows] <- NA
	ymax(object) - ((rownr-0.5) * yres(object))
}	

.yFromRow <- function(object, rownr) {
	if (rotated(object)) {
		stop('this function is not supported for rotated rasters')
	}
	ymax(object) - ((rownr-0.5) * yres(object))
}	
	


xFromCol <- function(object, colnr) {
	if (rotated(object)) {
		stop('this function is not supported for rotated rasters')
	}
	colnr <- round(colnr)
	colnr[colnr < 1 | colnr > object@ncols] <- NA
	xmin(object) + (colnr - 0.5) * xres(object) 
}  

.xFromCol <- function(object, colnr) {
	if (rotated(object)) {
		stop('this function is not supported for rotated rasters')
	}
	xmin(object) + (colnr - 0.5) * xres(object) 
 }  

	
cellFromXY <- function(object, xy) {
	if (inherits(xy, 'SpatialPoints')) {
		xy <- coordinates(xy)
		x <- xy[,1]
		y <- xy[,2]
	} else if (is.null(dim(xy))) { 
		x <- xy[1]
		y <- xy[2] 
	} else { 
		x <- xy[,1]
		y <- xy[,2] 
	}

	if (rotated(object)) {
		cr <- object@rotation@transfun(xy, inv=TRUE)
		cell <- (cr[,2]-1) * object@ncols + cr[,1]
	} else {
		rownr <- rowFromY(object, y) - 1
		colnr <- colFromX(object, x)
		cell <- rownr * object@ncols + colnr
	}
	return(cell)
}


colFromX <- function ( object, x )	{
	if (inherits(x, 'Spatial')) { 
		x <- x@coords[,1] 
	}
	if (rotated(object)) {
		stop('this function is not supported for rotated rasters')
	}
	colnr <- trunc((x - xmin(object)) / xres(object)) + 1 
	colnr[ x == xmax(object) ] <- object@ncols
	colnr[ x < xmin(object) | x > xmax(object) ] <- NA
	return(as.vector(colnr))
}

	
rowFromY <- function ( object, y )	{
	if (inherits(y, 'Spatial')) {
		y <- y@coords[,2] 
	}
	if (rotated(object)) {
		stop('this function is not supported for rotated rasters')
	}
	rownr <- 1 + (trunc((ymax(object) - y) / yres(object)))
	rownr[y == ymin(object) ] <- object@nrows 
	rownr[y > ymax(object) | y < ymin(object)] <- NA
	return(as.vector(rownr))
}	
	

xyFromCell <- function(object, cell, spatial=FALSE) {
	if (rotated(object)) {
		xy <- object@rotation@transfun( 
			cbind(colFromCell(object, cell), rowFromCell(object, cell)) 
		)
		colnames(xy) <- c("x", "y")	
	} else {
		xy <- matrix(data = NA, ncol=2, nrow=length(cell))
		colnames(xy) <- c("x", "y")	
		xy[,1] <- .xFromCol(object, .colFromCell(object, cell))
		xy[,2] <- .yFromRow(object, .rowFromCell(object, cell))
	}
	if (spatial) {
		xy <- SpatialPoints(xy, projection(object, asText=FALSE))
	}
	return(xy)
}  
	

	
	
if (!isGeneric("coordinates")) {
	setGeneric("coordinates", function(obj, ...)
		standardGeneric("coordinates"))
}	

		   
setMethod('coordinates', signature(obj='Raster'), 
    function(obj, ...){
		xyFromCell(obj, cell=1:ncell(obj), ...)
	}
)


yFromCell <- function(object, cell) {
	if (rotated(object)) {
		xy <- object@rotation@transfun(xy)
		return(xy[,2])
	} else {
		rows <- rowFromCell(object, cell)
		return( .yFromRow(object, rows) )
	}
}  
	
xFromCell <- function(object, cell) {
	if (rotated(object)) {
		xy <- object@rotation@transfun(xy)
		return(xy[,1])
	} else {
		cols <- colFromCell(object, cell)
		return( .xFromCol(object, cols) )
	}
}  

