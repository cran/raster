# author Robert Hijmans
# June 2010
# version 1.0
# license GPL3


.compareCRS <- function(x, y, unknown=FALSE, verbatim=FALSE, verbose=FALSE) {
	
	
	step1 <- function(z, verbatim) {
		if (class(z) == 'character') { 	# do nothing
		} else {
			z <- projection(z) # works for Raster and Spatial objects
		}
		z <- gsub(' ', '', z)
		if (!verbatim) {
			z <- unlist( strsplit(z, '+', fixed=TRUE) )[-1]
			z <- do.call(rbind, strsplit(z, '='))
		}
		z
	}

	x <- step1(x, verbatim)
	y <- step1(y, verbatim)
	
	if (verbatim) {
		return(x==y)
	}
	
	if (length(x) == 0 | length(y) == 0) {
		if (unknown) {
			return(TRUE)
		} else {
			if (verbose) {
				cat('Unknown CRS\n')
			}
			return(FALSE) 
		}
	}
	x <- x[which(x[,1] %in% y[,1]), ,drop=FALSE]
	y <- y[which(y[,1] %in% x[,1]), ,drop=FALSE]
	x <- x[order(x[,1]), ,drop=FALSE]
	y <- y[order(y[,1]), ,drop=FALSE]
	i <- x[,2] == y[,2]
	if (! all(i)) {
		if (verbose) {
			i <- which(!i)
			for (j in i) {
				cat('+',x[j,1], ':  ', x[j,2],' != ', y[j,2], '\n', sep='') 
			}
		}
		return(FALSE)
	}
	return(TRUE)
}


