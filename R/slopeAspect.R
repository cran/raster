# Author: Robert J. Hijmans
# Date : February 2010
# Version 1.0
# Licence GPL v3


slopeAspect <- function(dem, filename='', type='both', unit='', neighbors=8, flatAspect, ...) {
	type <- trim(tolower(type))
	stopifnot(type %in% c('', 'both' , 'slope', 'aspect'))
	unit <- trim(tolower(unit))
	stopifnot(unit %in% c('degrees', ''))
	filename <- trim(filename)
	stopifnot(neighbors %in% c(4, 8))
	stopifnot(projection(dem) != "NA")
	
	res <- res(dem)
	dx <- res[1]
	dy <- res[2]
	if (neighbors == 8) {
		fX <- matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3) / -8
		fY <- matrix(c(-1,0,1,-2,0,2,-1,0,1), nrow=3) / 8
	} else { # neighbors == 4
		fX <- matrix(c(0,-1,0,0,0,0,0,1,0), nrow=3) / -2
		fY <- matrix(c(0,0,0,-1,0,1,0,0,0), nrow=3) / 2
	}
	
	if (isLonLat(dem)) {
		dy <- pointDistance(cbind(0,0), cbind(0, dy), longlat=TRUE)
		fY <- fY / dy
		zy <- focalFilter(dem, fY)
		zx <- focalFilter(dem, fX)
		
		y <- yFromRow(dem, 1:nrow(dem))
		dx <- .haversine(-dx, y, dx, y) / 3
		zx <- t( t(zx) / dx)
		
	} else {
	
		fX <- fX / dx
		fY <- fY / dy
		zx <- focalFilter(dem, fX)
		zy <- focalFilter(dem, fY)
	}

	if (type == 'slope') {
		
		x <- sqrt( zy^2 + zx^2 ) 
		if (unit == 'degrees') {
			x <- atan(x) * (180 / pi)
		}
		layerNames(x) <- 'slope'
		
	} else if (type == 'aspect') {
		x <- atan2(zy, zx)
		x <- ((0.5*pi)-x) %% (2*pi)
		if (unit == 'degrees') {
			x <- x * (180/pi)
		}
		if (!missing (flatAspect)) {
			slope <-  sqrt( zy^2 + zx^2 ) 
			aspect <- overlay(x, slope, fun=function(x, y) { x[y==0] <- flatAspect; return(x) } )
		}
		layerNames(x) <- 'aspect'
		
	} else {
		x <- sqrt( zy^2 + zx^2 ) 
		aspect <- atan2(zy, zx) 
		aspect <- ((0.5*pi)-aspect) %% (2*pi)
		
		if (unit == 'degrees') {
			x <- atan(x) * (180/pi)
			aspect <- aspect * (180/pi)
		}
		if (!missing (flatAspect)) {
			aspect <- overlay(aspect, x, fun=function(x, y) { x[y==0] <- flatAspect; return(x) } )
		}
		
		layerNames(x) <- 'slope'
		layerNames(aspect) <- 'aspect'
		x <- stack(x, aspect)
	}

	if (filename != "") {
		x <- writeRaster(x, filename, ...)
	}
	return(x)
}

