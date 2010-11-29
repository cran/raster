# Author: Robert J. Hijmans  and Jacob van Etten
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.pointsToMatrix <- function(p) {
	if (inherits(p, 'SpatialPoints')) {
		p <- coordinates(p)
	} else if (is.data.frame(p)) {
		p <- as.matrix(p)
	} else if (is.vector(p)){
		if (length(p) != 2) {
			stop('Wrong length for a vector, should be 2')
		} else {
			p <- matrix(p, ncol=2) 
		}
	}
	if (is.matrix(p)) {
		if (ncol(p) != 2) {
			stop( 'A points matrix should have 2 columns')
		}
		cn <- colnames(p)
		if (length(cn) == 2) {
			if (toupper(cn[1]) == 'Y' | toupper(cn[2]) == 'X')  {
				stop('Highly suspect column names (x and y reversed?)')
			}
			if (toupper(substr(cn[1],1,3) == 'LAT' | toupper(substr(cn[2],1,3)) == 'LON'))  {
				stop('Highly suspect column names (longitude and latitude reversed?)')
			}
		}		
	} else {
		stop('points should be vectors of length 2, matrices with 2 columns, or a SpatialPoints* object')
	}

	return(p)
}


pointDistance <- function (point1, point2, longlat=FALSE,  ...) {

	type <- list(...)$type
	if (!is.null(type)) {
		if (!(type %in% c('Euclidean', 'GreatCircle'))) {
			stop('type should be Euclidean or GreatCircle')
		}
		if (type == 'Euclidean') { longlat <- FALSE } else { longlat <- TRUE }
		if (longlat) {
			# warning("type='Euclidean' is a depracated argument. Use 'longlat=TRUE'")
		} else {
			# warning("type='GreatCircle' is a depracated argument. Use 'longlat=FALSE'")
		}
	}		

	point1 <- .pointsToMatrix(point1)
	point2 <- .pointsToMatrix(point2)
	
	if(length(point1[,1]) != length(point2[,1])) {
		if(length(point1[,1]) > 1 & length(point2[,1]) > 1) {
			stop('point1 and point2 do not have the same number of rows; and neither has only a single row')
		}
	}
	
	if (! longlat ) {
		return ( sqrt(( point1[,1] -  point2[,1])^2 + (point1[,2] - point2[,2])^2) )
	} else { 
		return(.greatCircleDist(point1[,1], point1[,2], point2[,1], point2[,2], r=6378137) )
	}
}

.greatCircleDist <- function(x1, y1, x2, y2, r=6378137) {
	x1 <- x1 * pi / 180
	y1 <- y1 * pi / 180
	x2 <- x2 * pi / 180
	y2 <- y2 * pi / 180
	#cosd <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2)
	#return(r * acos(cosd))
	#  the following is supposedly more precise than above (http://en.wikipedia.org/wiki/Great_circle_distance)
	x <- sqrt((cos(y2) * sin(x1-x2))^2 + (cos(y1) * sin(y2) - sin(y1) * cos(y2) * cos(x1-x2))^2)
	y <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2)
	return ( r * atan2(x, y) )
}
