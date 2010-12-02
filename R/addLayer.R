# R package 'raster'
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


	
if (!isGeneric("addLayer")) {
	setGeneric("addLayer", function(x, ...)
		standardGeneric("addLayer"))
}	

setMethod('addLayer', signature(x='Raster'), 
function(x, ..., keepone=FALSE) {

	if (! inherits(x, 'RasterStack')) {
		x <- stack(x)
	}

	rasters <- .makeRasterList(..., keepone=keepone)
	if (length(rasters)==0) { return(x) }

	vals <- sapply(rasters, hasValues) 
	if (sum(vals) == 0 &  nlayers(x) == 0) { 
		vals[1] <- TRUE 
	}
	if (sum(vals) != length(vals)) { 
		warning('Cannot add RasterLayers with no associated data in memory or on disk to a RasterStack')
	}
	rasters <- rasters[vals]
	if (length(rasters)==0) { return(x) }
	
	if (nlayers(x) == 0) {
		r <- rasters[[1]]
		x@nrows <- r@nrows
		x@ncols <- r@ncols
		x@extent <- r@extent
		x@crs <- r@crs

		nl <- 1
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
		

	for (i in seq(along=rasters)) { 
		r <- rasters[[i]]
		if (!compare(x, r)) { 
			warning("could not add r:", filename(r))
			next
		}
		
		nl <- nlayers(x) + 1 
		count <- 1
		cname <- trim(r@layernames)
		if (cname == "") {
			cname <- paste("layer", nl, sep="")
		}
		cn <- cname
		for (j in 1:(nl-1)) {
			if ( cn == layerNames(x)[j] ) { 
				count <- count + 1 
				cn <- paste(cname, "_", count, sep="")
			}
		}	
		x@layernames[nl] <- cn
		
		if ( fromDisk(r) ) {
			r <- clearValues(r)
		}
		x@layers[nl] <- r 
	}
	
	return(x)
}	
)



