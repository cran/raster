# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3


yFromRow <- function(object, rownr) {
	rownr <- round(rownr)
	rownr[rownr < 1 | rownr > object@nrows] <- NA
	y <- ymax(object) - ((rownr-0.5) * yres(object))
	return(y) }	
	
	
xFromCol <- function(object, colnr) {
	colnr <- round(colnr)
	colnr[colnr < 1 | colnr > object@ncols] <- NA
	x <- xmin(object) + (colnr - 0.5) * xres(object) 
	return(x) }  


cellFromXY <- function(object, xy) {
	if (inherits(xy, 'Spatial')) {
		x <- xy@coords[,1]
		y <- xy@coords[,2]
	} else if (is.null(dim(xy))) { 
		x <- xy[1]
		y <- xy[2] 
	} else { 
		x <- xy[,1]
		y <- xy[,2] 
	}
	rownr <- rowFromY(object, y) - 1
	colnr <- colFromX(object, x)
	cell <- rownr * object@ncols + colnr
	return(cell)
}


colFromX <- function ( object, x )	{
	if (inherits(x, 'Spatial')) { 
		x <- x@coords[,1] 
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
	rownr <- 1 + (trunc((ymax(object) - y) / yres(object)))
	rownr[y == ymin(object) ] <- object@nrows 
	rownr[y > ymax(object) | y < ymin(object)] <- NA
	return(rownr)
}	
	

xyFromCell <- function(object, cell, spatial=FALSE) {
	xy <- matrix(data = NA, ncol=2, nrow=length(cell))
	xy[,1] <- colFromCell(object, cell)
	xy[,2] <- rowFromCell(object, cell)
	xy[,1] <- xFromCol(object, xy[,1])
	xy[,2] <- yFromRow(object, xy[,2]) 		
	colnames(xy) <- c("x", "y")
	if (spatial) {
		xy <- SpatialPoints(xy, projection(object, asText=FALSE))
	}
	return(xy)
}  
	

yFromCell <- function(object, cell) {
	cell <- rowFromCell(object, cell)
	return( yFromRow(object, cell) )
}  
	
xFromCell <- function(object, cell) {
	cell <- colFromCell(object, cell)
	return( xFromCol(object, cell) )
}  

