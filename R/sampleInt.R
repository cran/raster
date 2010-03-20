# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : Febrary 2009
# Version 0.9
# Licence GPL v3


sampleInt <- function(n, size) {
	
	n = round(n[1])
	size = round(size[1])
	
	if (n < 1) { stop('n should be >= 1') }
	if (size < 1) { stop('size should be >= 1') }
		
	if (size >= n) return(1:n)
	
	switched = FALSE
	if (size > (0.5 * n)) {
		switched = TRUE
		size = n - size
	}
	
	done = FALSE
	samp = NULL
	while (! done) {
		f = round(runif(size*1.1) * n)
		samp = unique(c(samp, f))
		if (length(samp) >= size) {
			samp = samp[1:size]
			done = TRUE
		}
	}
	if (switched) {
		return( (1:n)[-samp] )
	} else {
		return(samp)
	}
}
