# Author: Robert J. Hijmans
# Date : February 2010
# Version 1.0
# Licence GPL v3


.slopeAspect2 <- function(dem, filename='', type='both', unit='', neighbors=8, flatAspect, ...) {
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
	f <- matrix(1,3,3)
	
	if (isLonLat(dem)) {
		dy <- pointDistance(cbind(0,0), cbind(0, dy), longlat=TRUE)
		fY <- fY / dy
		
		fun1 <- function(x, ...) cbind(sum(x * fX), sum(x * fY))
		zx <- stack(focalFilter(dem, f, fun=fun1))
		zy <- zx[[2]]
		zx <- zx[[1]]

		y <- yFromRow(dem, 1:nrow(dem))
		dx <- .haversine(-dx, y, dx, y) / 3
		zx <- t( t(zx) / dx)
		
	} else {
		fX <- fX / dx
		fY <- fY / dy
		fun2 <- function(x, ...) cbind(sum(x * fX), sum(x * fY))
		zx <- focalFilter(dem, f, fun=fun2)
		zy <- zx[[2]]
		zx <- zx[[1]]
		
		#zx <- focalFilter(dem, fX)
		#zy <- focalFilter(dem, fY)
	}

	if (type == 'slope') {
		
		if (unit == 'degrees') {
			fun <- function(x,y) { atan(sqrt(x^2 + y^2)) * (180 / pi) }
		} else {
			fun <- function(x,y) { sqrt(x^2 + y^2) }
		}	
		x <- overlay(zx, zy, fun=fun, filename=filename, ...)
		layerNames(x) <- 'slope'

		
	} else if (type == 'aspect') {
		if (!missing (flatAspect)) {
			if (unit == 'degrees') {
				fun <- function(x,y){ 
					asp <- (((0.5*pi)-atan2(y, x)) %% (2*pi)) * (180/pi);
					slp <- sqrt(x^2 + y^2); 
					asp[slp==0] <- flatAspect;
					return(asp)
				}
			} else {
				fun <- function(x,y){ 
					asp <- (((0.5*pi)-atan2(y, x)) %% (2*pi))
					slp <- sqrt(x^2 + y^2); 
					asp[slp==0] <- flatAspect;
					return(asp)
				}
			}
		} else {
			if (unit == 'degrees') {
				fun <- function(x,y){ (((0.5*pi)-atan2(y, x)) %% (2*pi)) * (180/pi) }
			} else {
				fun <- function(x,y){ ((0.5*pi)-atan2(y, x)) %% (2*pi) }
			}
		}
		
		x <- overlay(zx, zy, fun=fun, filename=filename, ...)
		layerNames(x) <- 'aspect'
		
	} else {

		if (!missing (flatAspect)) {
			if (unit == 'degrees') {
				fun <- function(x,y){ 
					asp <- (((0.5*pi)-atan2(y, x)) %% (2*pi)) * (180/pi);
					slp <- sqrt(x^2 + y^2); 
					asp[slp==0] <- flatAspect;
					return(cbind(slp, asp))
				}
			} else {
				fun <- function(x,y){ 
					asp <- (((0.5*pi)-atan2(y, x)) %% (2*pi))
					slp <- sqrt(x^2 + y^2); 
					asp[slp==0] <- flatAspect;
					return(cbind(slp, asp))
				}
			}
		} else {
			if (unit == 'degrees') {
				fun <- function(x,y){ 
					asp <- (((0.5*pi)-atan2(y, x)) %% (2*pi)) * (180/pi);
					slp <- sqrt(x^2 + y^2); 
					return(cbind(slp, asp))
				}
			} else {
				fun <- function(x,y){ 
					asp <- (((0.5*pi)-atan2(y, x)) %% (2*pi))
					slp <- sqrt(x^2 + y^2); 
					return(cbind(slp, asp))
				}
			}
		}
		x <- overlay(zx, zy, fun=fun, filename=filename, ...)
		layerNames(x) <- c('slope', 'aspect')
	}

	return(x)
}


