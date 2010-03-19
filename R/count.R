# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


count <- function(raster, value, digits=0, progress) {
	if (canProcessInMemory(raster, 2)) {
		if (dataContent(raster) != 'all') {
			raster <- readAll(raster)
		}
		if (is.na(value)) {
			x <- sum(is.na(values(raster)))
		} else {
			v <- na.omit(round(values(raster), digits=digits))
			x <- sum(v == value)
		}
	} else {
		tr <- blockSize(raster, n=2)
		if (missing(progress)) progress = .progress()
		pb <- pbCreate(tr$n, type=progress)			
		x <- 0
		for (i in 1:tr@n) {
			v <- getValuesBlock(raster, row=tr$row[i], nrows=tr$nrows[i])
			if (is.na(value)) {
				x <- x + sum(is.na(v))
			} else {
				v <- na.omit(round(v, digits=digits))
				x <- x + sum(v == value)
			}
			pbStep(pb, i)
		}
		pbClose(pb)
	}
	return(x)
}


