# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : Febrary 2009
# Version 0.9
# Licence GPL v3


sampleInt <- function(n, size, replace=FALSE) {
	
	n = round(n[1])
	size = round(size[1])
	
	stopifnot(n > 0)
	stopifnot(size > 0)

	if (size > n & ! replace) {
		warning('size changed to n because to it cannot be larger than n when replace is FALSE')
		size <- n
	}
		
	if (!replace) {
	
		switched = FALSE
		if (size == n) { 
			res <- 1:n
			return(res[order(runif(length(res)))])
		} else {
			if (size > (0.5 * n)) {
				switched = TRUE
				size = n - size
			}
		}
		done = FALSE
		samp = NULL
		while (! done) {
			f = ceiling(runif(size * 1.1) * n)
			samp = unique(c(samp, f))
			if (length(samp) >= size) {
				samp = samp[1:size]
				done = TRUE
			}
		}
		if (switched) { 
			samp <- (1:n)[-samp] 
		}
		
	} else {
		samp <- ceiling(runif( size ) * n)
	}
	
	return( samp )
}
