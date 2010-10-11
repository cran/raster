# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3



if (!isGeneric("lineValues")) {
	setGeneric("lineValues", function(lns, x, ...)
		standardGeneric("lineValues"))
}	


setMethod("lineValues", signature(lns='SpatialLines', x='Raster'), 
function(lns, x, fun, ...) {
	spbb <- bbox(lns)
	rsbb <- bbox(x)
	addres <- max(res(x))
	nlns <- length( lns@lines )
	res <- list()
	res[[nlns+1]] = NA

	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		return(res[1:nlns])
	}
	
	rr <- raster(x)
	for (i in 1:nlns) {
		pp <- lns[i,]
		spbb <- bbox(pp)
		
		if (! (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) ) {
			rc <- crop(rr, extent(pp)+addres)
			rc <- linesToRaster(pp, rc, silent=TRUE)
			xy <- rasterToPoints(rc)[,-3]
			if (length(xy) > 0) { # always TRUE?
				res[[i]] <- xyValues(x, xy)
			} 
		}
	}
	
	res = res[1:nlns]
	
	if (! missing(fun)) {
		i <- sapply(res, is.null)
		j <- vector(length=length(i))
		j[i] <- NA
		if (nlayers(x) > 1) {
			j[!i] <- sapply(res[!i], function(x) apply(x, 2, fun))
		} else {
			j[!i] <- sapply(res[!i], fun)
		}
		res <- j
	}
	res
}
)


