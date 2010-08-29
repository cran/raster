# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

cellStats <- function(x, stat='mean', ...) {

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
	
	if (nlayers(x) == 1) {
		makeMat = TRUE
	} else {
		makeMat = FALSE
	}
	
	
	if (class(stat) != 'character') {
		if ( ! inMemory(x) ) {
			if (! canProcessInMemory(x)) {
				stop("RasterLayer is too large. You can use fun='sum', 'mean', 'min', 'max', 'sd', 'countNA', but not a function")
			}
		}
		x <- getValues(x)
		if (makeMat) x <- matrix(x, ncol=1)
		return( apply(x, 2, stat, na.rm=TRUE) )
		
	} else {

		st <- NULL
		counts <- FALSE
		if (stat == 'sum') {
			fun <- sum
			st <- 0	
		} else if (stat == 'min') {
			fun <- min
		} else if (stat == 'max') {
			fun <- max
		} else if (stat == 'countNA') {
			nc <- x@ncols
			st <- 0	
		} else if (stat == 'skew') {
			z <- 0
			st <- 0	
			zsd <- getzsd(x, ...)
			zmean <- getzmean(x, ...)
		} else if (stat == 'mean' | stat == 'sd') {
			st <- 0	
			sumsq <- 0
			cnt <- 0
		} else { 
			stop("invalid 'stat'. Should be 'sum', 'min', 'max', 'sd', 'mean', or 'countNA'") 
		}

		
		tr <- blockSize(x)
		pb <- pbCreate(tr$n, type=.progress())			
		
		for (i in 1:tr$n) {
			d <- getValues(x, row=tr$row[i], nrows=tr$size)
			if (makeMat) d <- matrix(d, ncol=1)

			nas <- apply(d, 2, function(x)sum(is.na(x) ))
			if (min(nas) == nrow(d)) { next }
			cells <- nrow(d) - nas
			
			if (stat == 'sd') {
				st <- apply(d, 2, sum, na.rm=TRUE) + st
				cnt <- cnt + cells
				sumsq <- apply( d^2 , 2, sum) + sumsq
			
			} else if (stat=='mean') {
				st <- apply(d, 2, sum, na.rm=TRUE) + st
				cnt <- cnt + cells
				
			} else if (stat=='countNA') {
				st <- st + nas
				
			} else if (stat=='skew') {
				d <- t( t(d) - zmean )^3
				st <- apply(d , 2, sum, na.rm=TRUE ) + st
				z <- z + cells
			} else {
				#simple additive functions such as sum, min, max
				st <- apply(rbind(d, st), 2, fun, na.rm=TRUE)
			}
			
			pbStep(pb, i) 
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

