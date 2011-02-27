# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


blockSize <- function(x, chunksize, n=nlayers(x), minblocks=4) {
	n = max(n, 1)
	if (missing(chunksize)) {
		bs = .chunksize()  / n
	} else {
		bs = chunksize
	}
	size <- min(nrow(x), max(1, floor(bs / ncol(x))))
	# min number of chunks
	if (size > 1) {
		minblocks = min(nrow(x), max(1, minblocks))
		size = min(ceiling(nrow(x)/minblocks), size)
	}
	nb <- ceiling(x@nrows / size)
	row <- (0:(nb-1))*size + 1
	nrows <- rep(size, length(row))

	dif = nb * size - nrow(x)
	nrows[length(nrows)] = nrows[length(nrows)] - dif
	
	return(list(size=size, row=row, nrows=nrows, n=nb))
}

