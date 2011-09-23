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
		i <- na.omit(.nameToIndex(subset, layerNames(x)))
		if (length(i)==0) {
			stop('invalid layer names')
		} else if (length(i) < length(subset)) {
			warning('invalid layer names omitted')
		}
		subset <- i
	}
	subset <- as.integer(subset)
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
		i <- na.omit(.nameToIndex(subset, layerNames(x)))
		if (length(i)==0) {
			stop('invalid layer names')
		} else if (length(i) < length(subset)) {
			warning('invalid layer names omitted')
		}
		subset <- i
	}
	
	
	subset <- as.integer(subset)
	nl <- nlayers(x)
	if (! all(subset %in% 1:nl)) {
		stop('not a valid subset')
	}
	if (nl==1) {
		return(x)
	}
	
	varname <- attr(x@data, "zvar")
	if (is.null(varname)) { varname <- "" }
	
	if (fromDisk(x)) {
		if (drop & length(subset)==1) {
			return( raster(filename(x), band=subset, varname=varname) )
		} else {
			return( stack(filename(x), bands=subset, varname=varname) )
		}
	} else {
		if (hasValues(x)) {
			x@data@values <- x@data@values[, subset, drop=FALSE]
			x@layernames <- x@layernames[subset]
			x@data@min <- x@data@min[subset]
			x@data@max <- x@data@max[subset]
		} 
		x@data@nlayers <- as.integer(length(subset))
		return(x)
	}
} )


