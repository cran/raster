# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3


yFromRow <- function(object, rownr) {
	object <- raster(object)
	rownr <- round(rownr)
	rownr[rownr < 1 | rownr > nrow(object)] <- NA
	y <- ymax(object) - ((rownr-0.5) * yres(object))
	#hello
	return(y) }	
	
	
xFromCol <- function(object, colnr) {
	object <- raster(object)
	colnr <- round(colnr)
	colnr[colnr < 1 | colnr > ncol(object)] <- NA
	x <- xmin(object) + (colnr - 0.5) * xres(object) 
	return(x) }  


cellFromXY <- function(object, xy) {
	object <- raster(object)
	if (class(xy) == 'SpatialPoints' | class(xy) == 'SpatialPointsDataFrame') {
		x <- coordinates(xy)[,1]
		y <- coordinates(xy)[,2]
	} else if (is.null(dim(xy))) { 
		x <- xy[1]
		y <- xy[2] 
	} else { 
		x <- xy[,1]
		y <- xy[,2] 
	}
	rownr <- rowFromY(object, y) - 1
	colnr <- colFromX(object, x)
	cell <- rownr * ncol(object) + colnr
	return(cell)
}


colFromX <- function ( object, x )	{
	object <- raster(object)
	if (class(x) == 'SpatialPoints' | class(x) == 'SpatialPointsDataFrame') {	x <- x@points[,1] }
	colnr <- (trunc((x - xmin(object)) / xres(object))) + 1 
	colnr[x == xmax(object)] <- ncol(object)
	colnr[x < xmin(object) | x > xmax(object) ] <- NA
	return(as.vector(colnr))
}
	
	
rowFromY <- function ( object, y )	{
	object <- raster(object)
	if (class(y) == 'SpatialPoints' | class(y) == 'SpatialPointsDataFrame') {	y <- y@points[,2] }
	rownr <- 1 + (trunc((ymax(object) - y) / yres(object)))
	rownr[y == ymin(object) ] <- nrow(object) 
	rownr[y > ymax(object) | y < ymin(object)] <- NA
	return(rownr)
}	
	

xyFromCell <- function(object, cell, asSpatialPoints=FALSE) {
	object <- raster(object)
	cell <- round(cell)
	xy <- matrix(data = NA, ncol=2, nrow=length(cell))
	colnr <- colFromCell(object, cell)
	rownr <- rowFromCell(object, cell)
	xy[,1] <- xFromCol(object, colnr)
	xy[,2] <- yFromRow(object, rownr) 		
	colnames(xy) <- c("x", "y")
	if (asSpatialPoints) {
		xy <- SpatialPoints(xy, projection(object, asText=FALSE))
	}
	return(xy)
}  
	

yFromCell <- function(object, cell) {
	object <- raster(object)
	cell <- round(cell)
	rownr <- rowFromCell(object, cell)
	y <- yFromRow(object, rownr) 		
	return(y)
}  
	
xFromCell <- function(object, cell) {
	object <- raster(object)
	cell <- round(cell)
	colnr <- colFromCell(object, cell)
	x <- xFromCol(object, colnr)
	return(x)
}  

	
