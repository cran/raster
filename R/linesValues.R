# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3




lineValues <- function(lns, x, ...) {
	.warnExtract(6)
	extract(x, lns, ...)
}


.lineValues <- function(x, lns, fun, na.rm=FALSE, cellnumbers=FALSE, ...) {
	spbb <- bbox(lns)
	rsbb <- bbox(x)
	addres <- 2 * max(res(x))
	nlns <- length( lns@lines )
	res <- list()
	res[[nlns+1]] = NA
	

	if (cellnumbers) {
		if (!missing(fun)) {
			cellnumbers <- FALSE
			warning("'cellnumbers=TRUE' ignored when argument 'fun' is used")
		}
	}
	
	if (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) {
		return(res[1:nlns])
	}
	
	rr <- raster(x)
	cn <- layerNames(x)
	if (cn == "") { cn <- paste('v', 1:nlayers(x), sep='') }
	for (i in 1:nlns) {
		pp <- lns[i,]
		spbb <- bbox(pp)
		
		if (! (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) ) {
			rc <- crop(rr, extent(pp)+addres)
			rc <- .linesToRaster(pp, rc, silent=TRUE)
			xy <- rasterToPoints(rc)[,-3,drop=FALSE]
			if (cellnumbers) {
				if (length(xy) > 0) { # always TRUE?
					v <- cbind(cellFromXY(rr, xy), .xyValues(x, xy))
					colnames(v) <- c('cell', cn)
					res[[i]] <- v
				}
			} else {
				if (length(xy) > 0) { # always TRUE?
					res[[i]] <- .xyValues(x, xy)
				}
			} 
		}
	}
	
	res = res[1:nlns]
	
	if (! missing(fun)) {
		i <- sapply(res, is.null)
		if (nlayers(x) > 1) {
			j <- matrix(ncol=nlayers(x), nrow=length(res))
			j[!i] <- t(sapply(res[!i], function(x) apply(x, 2, fun, na.rm=na.rm)))
			colnames(j) <- layerNames(x)
		} else {
			j <- vector(length=length(i))
			j[i] <- NA
			j[!i] <- sapply(res[!i], fun, na.rm)
		}
		res <- j
	}
	res
}



