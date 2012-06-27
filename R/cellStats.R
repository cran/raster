# Author: Robert J. Hijmans
# Date : March 2009 / April 2012
# Version 1.0
# Licence GPL v3



.csTextFun <- function(fun) {
	if (class(fun) != 'character') {
		if (is.primitive(fun)) {
			test <- try(deparse(fun)[[1]], silent=TRUE)
			if (test == '.Primitive(\"sum\")') { fun <- 'sum' 
			} else if (test == '.Primitive(\"min\")') { fun <- 'min' 
			} else if (test == '.Primitive(\"max\")') { fun <- 'max' 
			}
		} else {
			f <- paste(deparse(fun), collapse = "\n")
			if (f == paste(deparse(mean), collapse = "\n")) {
				fun <- 'mean' 
			} else if (f == paste(deparse(sd), collapse = "\n")) {
				fun <- 'sd' 
			} else if (f == paste(deparse(range), collapse = "\n")) {
				fun <- 'range' 
			} 			
		} 
	}
	return(fun)
}


	
if (!isGeneric("cellStats")) {
	setGeneric("cellStats", function(x, stat, ...)
		standardGeneric("cellStats"))
}	


setMethod('cellStats', signature(x='RasterStackBrick'),
	function(x, stat='mean', na.rm=TRUE, asSample=TRUE, ...) {
	
		stopifnot(hasValues(x))

		makeMat <- FALSE
		if (nlayers(x) == 1) {	
			makeMat <- TRUE
			#return( cellStats(raster(x, values=TRUE, stat=stat, ...) )		
		}
	
		stat <- .csTextFun(stat)
	
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
					return( colMeans(x, na.rm=na.rm) )
			
				} else if (stat == "sum" ) {
					return( colSums(x, na.rm=na.rm) )
					
				} else if (stat == 'countNA') { 
					return( colSums(is.na(x)) )
				
				} else if (stat == 'sd') { 
					
					st <- apply(x, 2, sd, na.rm=na.rm) 
					if (! asSample) {
						if (na.rm) {
							n <- colSums(! is.na(x))
						} else {
							n <- nrow(x)
						}
						st <- n * st / (n-1)
					} 
					return(st)
				}
			} 
			return( ( apply(x, 2, stat, na.rm=na.rm) ) )
		}
		
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
			st <- 0	
			counts <- TRUE
		} else if (stat == 'skew') {
			
			zmean <- cellStats(x, 'mean')
			cnt <- 0
			st <- 0	
			stsd <- 0
			sumsq <- 0
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
				nas <- colSums( is.na(d) )
				if (stat != 'countNA') {
					if (min(nas) == nrow(d)) { 
						next 
					}
					cells <- nrow(d) - nas
				}
			}
				
			if (stat=='mean') {
				st <- colSums(d, na.rm=na.rm) + st
				cnt <- cnt + cells
			
			} else if (stat=='sum') {
				st <- colSums(d, na.rm=na.rm) + st

			} else if (stat == 'sd') {
				st <- colSums(d, na.rm=na.rm) + st
				cnt <- cnt + cells
				sumsq <- colSums(d^2, na.rm=na.rm) + sumsq

			} else if (stat=='countNA') {
				st <- st + nas
					
			} else if (stat=='skew') {
			
				stsd <- colSums(d, na.rm=na.rm) + stsd
				sumsq <- colSums( d^2, na.rm=na.rm) + sumsq
				d <- t( t(d) - zmean )^3
				st <- colSums(d, na.rm=na.rm) + st
				cnt <- cnt + cells

			} else {
					# min, max, range
				st <- apply(rbind(d, st), 2, fun, na.rm=na.rm)
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
	
			meansq <- (stsd/cnt)^2
			stsd <- sqrt( (sumsq / cnt) - meansq )
			if (asSample) {
				stsd <- stsd * (cnt/(cnt-1))
			}
			st <- st / (cnt * stsd^3)			
			
		}
		
		pbClose(pb)
		return(st)
	}
)






setMethod('cellStats', signature(x='RasterLayer'),
	function(x, stat='mean', na.rm=TRUE, asSample=TRUE, ...) {
	
		stopifnot(hasValues(x))
		stat <- .csTextFun(stat)
	
		if (!inMemory(x)) {
			if (canProcessInMemory(x)) {
				x <- readAll(x)
			}
		}
		if (inMemory(x) ) {
			x <- getValues(x)

			if (class(stat) == 'character') {
				if (stat == "mean" ) {
					return( mean(x, na.rm=na.rm) )
				} else if (stat == "sum" ) {
					return( sum(x, na.rm=na.rm) )
				} else if (stat == 'countNA') { 
					return( sum(is.na(x)) )
				} else if (stat == "range" ) {
					return( range(x, na.rm=na.rm) )
				} else if (stat == "min" ) {
					return( min(x, na.rm=na.rm) )
				} else if (stat == "max" ) {
					return( max(x, na.rm=na.rm) )
				} else if (stat == "sd" ) {
					st <- sd(x, na.rm=na.rm)
					if (! asSample) {
						if (na.rm) {
							n <- length(na.omit(x))
						} else {
							n <- length(x)
						}
						st <- n * st / (n-1)
					} 
					return(st)
				} else if (stat == "skew" ) {
					if (na.rm) {
						x <- na.omit(x)
					}
					return( sum( (x - mean(x))^3 ) / (length(x) * sd(x)^3) )
				}
			} else {
				return( stat(x, na.rm=na.rm) )
			}
		}
		
		
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
			st <- 0	
			counts <- TRUE
		} else if (stat == 'skew') {
			zmean <- cellStats(x, 'mean')
			cnt <- 0
			st <- 0	
			stsd <- 0
			sumsq <- 0
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
			if (counts) {
				nas <- sum(is.na(d) )
				if (stat != 'countNA') {
					if (nas == length(d)) { # only NAs 
						next 
					}
					cells <- length(d) - nas
				}
			}
				
			if (stat=='mean') {
				st <- sum(d, na.rm=na.rm) + st
				cnt <- cnt + cells
			
			} else if (stat=='sum') {
				st <- sum(d, na.rm=na.rm) + st

			} else if (stat == 'sd') {
				st <- sum(d, na.rm=na.rm) + st
				cnt <- cnt + cells
				sumsq <- sum( d^2 , na.rm=na.rm) + sumsq

			} else if (stat=='countNA') {
				st <- st + nas
					
			} else if (stat=='skew') {
				
				stsd <- sum(d, na.rm=na.rm) + stsd
				sumsq <- sum( d^2 , na.rm=na.rm) + sumsq
				d <- (d - zmean)^3
				st <- sum(d, na.rm=na.rm) + st
				cnt <- cnt + cells
				
			} else if (stat=='min') {
				st <- min(d, st, na.rm=na.rm)
			} else if (stat=='max') {
				st <- max(d, st, na.rm=na.rm)
			} else if (stat=='range') {
				st <- range(d, st, na.rm=na.rm)
			}
				
			pbStep(pb, i) 
		}
		pbClose(pb)			
			
		if (stat == 'sd') {
			meansq <- (st/cnt)^2
			st <- sqrt( (sumsq / cnt) - meansq )
			if (asSample) {
				# n-1, as in sd 
				st <- st * (cnt/(cnt-1))
			}
		} else if (stat == 'mean') {
			st <- st / cnt
		} else if (stat == 'skew') {
			meansq <- (stsd/cnt)^2
			stsd <- sqrt( (sumsq / cnt) - meansq )
			if (asSample) {
				stsd <- stsd * (cnt/(cnt-1))
			}
			st <- st / (cnt * stsd^3)
		}
		
		return(st)
	}
)

