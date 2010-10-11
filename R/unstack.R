# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
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
	for (i in 1:nlayers(x)) {
		rlist <- c(rlist, raster(x, i))
	}
	return(rlist)
} )

