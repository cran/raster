# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("unstack")) {
	setGeneric("unstack", function(x, ...)
		standardGeneric("unstack"))
}	


setMethod("unstack", signature(x='RasterStack'), 
function(x) {
	return(x@layers)
} )


setMethod("unstack", signature(x='RasterBrick'), 
function(x) {
	rlist <- list()
	if (nlayers(x) == 0) { return(rlist) }
	r <- raster(x, 1)
	for (i in 1:nlayers(x)) {
		r@data@band <- i
		rlist <- c(rlist, r)
	}

	if (! fromDisk(x) ) { 
		for (i in 1:nlayers(x)) {
			rlist[i] <- setValues(rlist[[i]], x@data@values[,i])
		}
	}
	
	return(rlist)
} )

