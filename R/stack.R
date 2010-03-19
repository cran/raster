# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("stack")) {
	setGeneric("stack", function(x, ...)
		standardGeneric("stack"))
}	

setMethod("stack", signature(x='missing'), 
function(x) {
	return(new("RasterStack"))
	}
)

setMethod("stack", signature(x='Raster'), 
function(x, ..., bands=NULL) {
	rlist <- .makeRasterList(x, ..., keepone=FALSE)
	return(stack(rlist, bands))	
} )





setMethod("stack", signature(x='character'), 
function(x, ..., bands=NULL, xvar='', yvar='', zvar='', time='') {
    if (xvar != '' | yvar != '' | zvar != '' | is.numeric(time)) {
		return(.rasterFromCDF(x, type='RasterStack', xvar, yvar, zvar, time))
	} else {
		rlist <- c(x, list(...))
		return(stack(rlist, bands))
	}
} )


setMethod("stack", signature(x='list'), 
function(x, bands=NULL) {
	j <- 0
	r <- list()
	if (is.null(bands)) { bands = rep(-1, length(x)) }
	
	for (i in seq(along=x)) {
		j <- j + 1
		if (is.character(x[[i]])) {
#			if (is.null(bands)) {
#				r[j] <- raster(x[[i]])
#			} else if (bands[[i]] > 0) {
			if (bands[[i]] > 0) {
				r[j] <- raster(x[[i]], band=bands[[i]])
				if (length(bands) > 1 & length(x) == 1) {
					# single file, multuple bands
					for (q in 2:length(bands)) {
						r[q] <- raster(x[[i]], band=bands[[q]])
					}
				}
			} else {
				# all bands
				r[j] <- raster(x[[i]], band=1)
				bds <- nbands(r[[j]])
				if (bds > 1) {
					for (b in 2:bds) {
						j <- j + 1
						r[j] <- raster(x[[i]], band=b)
					}
				}
			}
		} else if (extends(class(x[[i]]), "Raster")) {
			r[j] <- x[[i]]
		} else {
			stop("Arguments should be Raster* objects or filenames")	
		}
	}
	return(addLayer(new("RasterStack"), r))
} )


setMethod("stack", signature(x='SpatialGridDataFrame'), 
	function(x) {
		stk <- new("RasterStack")
		if (class(x)=='SpatialGridDataFrame') {
			for (i in 1:ncol(x@data)) {
				rs <- raster(x, i)
				stk <- addLayer(stk, rs)
			}
		}
		return(stk)
	}
)
	

setMethod("stack", signature(x='SpatialPixelsDataFrame'), 
	function(x) {
		x <- as(x, 'SpatialGridDataFrame')
		return(stack(x))
	}
)
