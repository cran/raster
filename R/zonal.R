# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

zonal <- function(raster, zones, stat='mean', na.rm=TRUE, progress) {
	compare(c(raster, zones))
	
	if (missing(progress)) {progress <- .progress()}

	if (class(stat) != 'character') {
		if (canProcessInMemory(raster, 3)) {
			d <- getValues(raster)
			rm(raster)
			d <- cbind(d, round(getValues(zones)))
			rm(zones)
			if (na.rm) {d <- na.omit(d)	}
			alltab  <-  tapply(d[,1], d[,2], stat) 
			stat <- deparse(substitute(stat))
		} else {
			stop("RasterLayers are too large. You can use fun='sum', 'mean', 'min', or 'max', but not a function")
		}
	} else {

		counts <- FALSE
		if (stat == 'sum') {
			fun <- sum
		} else if (stat == 'min') {
			fun <- min
		} else if (stat == 'max') {
			fun <- max
		} else if (stat == 'mean') {
			fun <- sum
			counts <- TRUE
		} else { 
			stop("invalid 'stat', should be 'sum', 'min', 'max', or 'mean'") 
		}

		alltab <- array(dim=0)
		cnttab <- alltab
	
		tr <- blockSize(raster, n=2)
		pb <- pbCreate(tr$n, type=.progress())			
		
		for (i in 1:tr$n) {
			d <- getValuesBlock(raster, row=tr$row[i], nrows=tr$nrows[i])
			d <- cbind(d,  getValuesBlock(zones, row=tr$row[i], nrows=tr$nrows[i]))
			if (na.rm) { d <- na.omit(d)	}
			if (length(d) == 0) { next }
			alltab <- c(alltab, tapply(d[,1], d[,2], fun))
			if (counts) {
				cnttab <- c(cnttab, tapply(d[,1], d[,2], length))
			}
			if (length(alltab) > 10000) {
				groups <- as.integer(names(alltab))
				alltab <- tapply(as.vector(alltab), groups, fun)
				if (counts) {
					cnttab <- tapply(as.vector(cnttab), groups, sum)
				}
			}
			pbStep(pb, i)
		}
		pbClose(pb)
			
		groups <- as.integer(names(alltab))
		alltab <- tapply(as.vector(alltab), groups, fun)
		if (counts) {
			cnttab <- tapply(as.vector(cnttab), groups, sum)
			alltab <- alltab / cnttab
		}
	}
	zone <- as.integer(names(alltab))
	alltab <- data.frame(zone, as.numeric(alltab))
	colnames(alltab) <- c('zone', stat)
	return(alltab)
}


