# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  March 2011
# Version 1
# Licence GPL v3


stackSelect <- function(x, y, filename='', ...) {

	nl <- nlayers(x)
	stopifnot(nlayers(y) == 1)
	
	filename <- trim(filename)

	out <- raster(x)
	compare(out, y)
	
	if (canProcessInMemory(x, nl+1)) {
		i <- round(getValues(y))
		i[i < 1 | i > nl] <- NA
		x <- getValues(x)
		x <- x[cbind(1:nrow(x), i)]
		out <- setValues(out, x)
		if (filename != "") {
			out <- writeRaster(out, filename=filename, ...)
		}
		return(out)
	}
	
	if (filename == '') { filename <- rasterTmpFile() } 
	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out, n=nl+1)
	pb <- pbCreate(tr$n, type=.progress(...))

	for (i in 1:tr$n) {
		j <- round(getValues(y, row=tr$row[i], nrows=tr$nrows[i]))
		j[j < 1 | j > nl] <- NA
		v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		v <- v[cbind(1:nrow(v), j)]
		out <- writeValues(out, v, tr$row[i])
		pbStep(pb) 
	}

	out <- writeStop(out)
	pbClose(pb)
	return(out)
}
	

