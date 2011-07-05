# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  August 2009
# Version 1.0
# Licence GPL v3



if (!isGeneric('subset')) {
	setGeneric('subset', function(x, ...)
		standardGeneric('subset')) 
}


setMethod('subset', signature(x='RasterStack'), 
function(x, subset, drop=TRUE, ...) {
	if (is.character(subset)) {
		subset = .nameToIndex(subset, layerNames(x))
	}
	if (! all(subset %in% 1:nlayers(x))) {
		stop('not a valid subset')
	}
	if (length(subset) == 1 & drop) {
		x <- x@layers[[subset]]
	} else {
		x@layers <- x@layers[subset]
		x@layernames <- x@layernames[subset]
	}
	
	return(x)	
} )


setMethod('subset', signature(x='Raster'),
function(x, subset, drop=TRUE, ...) {
	if (is.character(subset)) {
		subset = .nameToIndex(subset, layerNames(x))
	}
	if (! all(subset %in% 1:nlayers(x))) {
		stop('not a valid subset')
	}
	subset <- as.integer(subset)
	varname <- attr(x@data, "zvar")
	if (is.null(varname)) { varname <- "" }
	
	if (fromDisk(x)) {
		if (drop & length(subset)==1) {
			return( raster(filename(x), bands=subset, varname=varname) )
		} else {
			return( stack(filename(x), bands=subset, varname=varname) )
		}
	} else {
		if (hasValues(x)) {
			x@data@values <- x@data@values[,subset]
			x@layernames <- x@layernames[subset]
		} 
		x@data@nlayers <- as.integer(length(subset))
		return(x)
	}
} )


