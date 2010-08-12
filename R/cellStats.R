# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

cellStats <- function(raster, stat='mean', ...) {
	getzmean <- function(raster, ..., zmean) {
		if (missing(zmean)) { 
			cellStats(raster, 'mean')
		} else {
			return(zmean)	
		}
	}
	getzsd <- function(raster, ..., zsd) {
		if (missing(zsd)) { 
			cellStats(raster, 'sd')
		} else {
			return(zsd)	
		}
	}
	if (class(stat) != 'character') {
		if ( inMemory(raster) ) { n <- 1 } else {n <- 2}
		if (canProcessInMemory(raster, n)) {
			d <- na.omit(getValues(raster))
			return( stat(d) )
		} else {
			stop("RasterLayer is too large. You can use fun='sum', 'mean', 'min', or 'max', but not a function")
		}
	} else {

		st  <- NULL
		counts <- FALSE
		if (stat == 'sum') {
			fun <- sum
		} else if (stat == 'min') {
			fun <- min
		} else if (stat == 'max') {
			fun <- max
		} else if (stat == 'countNA') {
			st <- 0		
			nc <- ncol(raster)
		} else if (stat == 'skew') {
			st <- 0
			z <- 0
			zsd <- getzsd(raster, ...)
			zmean <- getzmean(raster, ...)
		} else if (stat == 'mean' | stat == 'sd') {
			# do nothing
		} else { 
			stop("invalid 'stat'. Should be 'sum', 'min', 'max', 'sd', 'mean', 'skew' or 'countNA'") 
		}

		cnt <- 0
		sumsq <- 0
		
		pb <- pbCreate(nrow(raster), type=.progress(...))
		for (r in 1:nrow(raster)) {
			d <- na.omit(getValues(raster, r))
			if (length(d) == 0) { next }
			if (stat == 'sd') {
				st <- sum(d, st)
				cnt <- cnt + length(d)
				sumsq <- sum(d^2, sumsq)
			} else if (stat=='mean') {
				st <- sum(d, st)
				cnt <- cnt + length(d)
			} else if (stat=='countNA') {
				st <- st + (nc - length(d))
			} else if (stat=='skew') {
				st <- st + sum((d - zmean) ^3)
				z <- z + length(d)
			} else {
				st <- fun(c(d, st))
			}
			pbStep(pb, r) 
		}
		if (stat == 'sd') {
			meansq <- (st/cnt)^2
			st <- sqrt( (1 / cnt) * sumsq - meansq )
		} else if (stat == 'mean') {
			st <- st / cnt
		} else if (stat == 'skew') {
			st <- ((st / zsd)^3)/ z
		}
		pbClose(pb)
		return(st)
	}
}

