# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

cellStats <- function(x, stat='mean', ...) {

	stopifnot(hasValues(x))

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
	
	stdev <- function(x, na.rm=TRUE) {
		if (na.rm) {
			x <- na.omit(x)
		}
		sqrt(mean((x-mean(x))^2))
	}
	
	if (nlayers(x) == 1) {	makeMat = TRUE 	} else { makeMat = FALSE }
	

	
	stat <- .makeTextFun(stat)
	tryinmem <- TRUE
	if (class(stat) == 'character') {
		if (! stat %in% c('mean', 'sum', 'min', 'max', 'range', 'sd', 'countNA')) {
			tryinmem <- FALSE
		}
	}



	if (tryinmem) {
		if (!inMemory(x)) {
			if (canProcessInMemory(x)) {
				x <- readAll(x)
			}
		}
		if (inMemory(x) ) {
			x <- getValues(x)
			if (makeMat) {
				x <- matrix(x, ncol=1)
			}

			if (class(stat) == 'character') {
				if (stat == "mean" ) {
					return( colMeans(x, na.rm=TRUE) )
				
				} else if (stat == "sum" ) {
					return( colSums(x, na.rm=TRUE) )
				
				} else if (stat == 'min') { stat <- min 
				} else if (stat == 'max') { stat <- max 
				} else if (stat == 'sd')  { stat <- sd 
				} else if (stat == 'countNA') { stat <- function(x, na.rm){ sum(is.na(x)) } 
				} else {
					stop('stat (character type) unknown')
				}	
			} 
			return( ( apply(x, 2, stat, na.rm=TRUE) ) )
		}
	} 
	
	#stat <- .makeTextFun(stat)
	if (class(stat) != 'character') {
		stop('cannot use this function for large files')
	}
	
	st <- NULL
	counts <- FALSE
	if (stat == 'sum') {
		fun <- sum
		st <- 0	
	} else if (stat == 'min') {
		fun <- min
	} else if (stat == 'max') {
		fun <- max
	} else if (stat == 'range') {
		fun <- range
	} else if (stat == 'countNA') {
		nc <- x@ncols
		st <- 0	
		counts <- TRUE
	} else if (stat == 'skew') {
		z <- 0
		st <- 0	
		zsd <- getzsd(x, ...)
		zmean <- getzmean(x, ...)
		counts <- TRUE
	} else if (stat == 'mean' | stat == 'sd') {
		st <- 0	
		sumsq <- 0
		cnt <- 0
		counts <- TRUE
	} else { 
		stop("invalid 'stat'. Should be sum, min, max, sd, mean, or 'countNA'") 
	}

		
	tr <- blockSize(x)
	pb <- pbCreate(tr$n)			
	
	for (i in 1:tr$n) {
		d <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		if (makeMat) {
			d <- matrix(d, ncol=1)
		}
		if (counts) {
			nas <- apply(d, 2, function(x)sum(is.na(x) ))
			if (stat != 'countNA') {
				if (min(nas) == nrow(d)) { next }
				cells <- nrow(d) - nas
			}
		}
			
		if (stat=='mean') {
			st <- colSums(d, na.rm=TRUE) + st
			cnt <- cnt + cells
		
		} else if (stat=='sum') {
			st <- colSums(d, na.rm=TRUE) + st

		} else if (stat == 'sd') {
			st <- colSums(d, na.rm=TRUE) + st
			cnt <- cnt + cells
			sumsq <- apply( d^2 , 2, sum, na.rm=TRUE) + sumsq

		} else if (stat=='countNA') {
			st <- st + nas
				
		} else if (stat=='skew') {
			d <- t( t(d) - zmean )^3
			st <- colSums(d, na.rm=TRUE) + st
			z <- z + cells
		} else {
				# min, max
			st <- apply(rbind(d, st), 2, fun, na.rm=TRUE)
		}
			
		pbStep(pb, i) 
	}
		
		
	if (stat == 'sd') {
		meansq <- (st/cnt)^2
		# cnt/(cnt-1) to use n-1, as in sd 
		st <- sqrt(( (sumsq / cnt) - meansq ) * (cnt/(cnt-1)))
	} else if (stat == 'mean') {
		st <- st / cnt
	} else if (stat == 'skew') {
		st <- ((st / zsd)^3)/ z
	}
	
	pbClose(pb)
	return(st)
}

