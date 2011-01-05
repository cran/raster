# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("addLayer")) {
	setGeneric("addLayer", function(x, ...)
		standardGeneric("addLayer"))
}	

setMethod('addLayer', signature(x='Raster'), 
function(x, ...) {

	if (! inherits(x, 'RasterStack')) {
		x <- stack(x)
	}

	rasters <- .makeRasterList(...)
	if (length(rasters)==0) { 
		return(x) 
	}

	compare(rasters) 
		
	vals <- sapply(rasters, hasValues) 
	if (sum(vals) == 0 &  nlayers(x) == 0) { 
		vals[1] <- TRUE 
	}
	if (sum(vals) != length(vals)) { 
		warning('Cannot add RasterLayer with no associated data in memory or on disk to a RasterStack')
	}
	rasters <- rasters[vals]
	
	if (nlayers(x) == 0) {
		r <- rasters[[1]]
		x@nrows <- r@nrows
		x@ncols <- r@ncols
		x@extent <- r@extent
		x@crs <- r@crs

		nl <- 1
		r@layernames <- trim(as.character(r@layernames))
		if (trim(r@layernames) != "") {
			cname <- trim(r@layernames)
		} else {
			cname <- "layer1"
		}
		x@layernames[1] <- cname
		x@layers[nl] <- r 
		
		rasters <- rasters[-1]
		if (length(rasters)==0) { return(x) }
	}
		

	x@layers <- c(x@layers, rasters)
	lyrns <- sapply(x@layers, layerNames)
	layerNames(x) <- .makeUniqueNames(lyrns)

	return(x)
}	
)



