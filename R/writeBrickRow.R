# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3


.startBrickRowWriting <- function(x, bandorder, filename, ...) {
	datatype <- .datatype(...)
	dataType(x) <- datatype
	overwrite <- .overwrite(...)
	
	filename <- trim(filename)
	if (filename == '') { stop('provide a filename') }
	
	if (bandorder == 'BSQ') { stop('cannot use BSQ in row by row writing')	}
	if (!bandorder %in% c('BIL', 'BIP')) { stop("invalid bandorder, should be 'BIL' or 'BIP'") }
	
	x@file@bandorder <- bandorder
	x@file@nbands <- nlayers(x)

	filename(x) <- filename
	
	fnamevals <- .setFileExtensionValues(filename, "raster")
	if (!overwrite & (file.exists(filename) | file.exists(fnamevals))) {
		stop(paste(filename,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
	}

	attr(x@file, "con") <- file(fnamevals, "wb")
	attr(x@file, "dsize") <- dataSize(x@file@datanotation)
	attr(x@file, "dtype") <- .shortDataType(x@file@datanotation)

	x@file@driver <- 'raster'
	
	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))
	x@data@haveminmax <- FALSE
	return(x)
}


.stopBrickRowWriting <- function(x, ...) {
	filetype <- .filetype(...)  # not used
	writeRasterHdr(x, filetype) 
	close(x@file@con)
	x@data@haveminmax <- TRUE
	if (x@file@dtype == "INT") {
		x@data@min <- round(x@data@min)
		x@data@max <- round(x@data@max)
	} else if ( x@file@dtype =='LOG' ) { 
#		x@data@min <- as.logical(x@data@min)
#		x@data@max <- as.logical(x@data@max)
	}
	x@data@source <- 'disk'
	x@data@content <- 'nodata'
#	x@data@values <- matrix(nrow=0, ncol=0)
	return(x)
}		


.writeBrickRow <- function(object, filename, bandorder='BIL', ...) {
	if (dataIndices(object)[1] == 1) { 
		object <- .startBrickRowWriting(object, bandorder=bandorder, filename=filename, ...)
 	} 
	
	object@data@values[is.nan(object@data@values)] <- NA
	object@data@values[is.infinite(object@data@values)] <- NA
	values <- object@data@values
	values[is.na(values)] <- Inf
	object@data@min <- pmin(object@data@min,  apply(values, 2, function(x){min(x, na.rm=T)}))
	values[!is.finite(values)] <- -Inf
	object@data@max <- pmax(object@data@max,  apply(values, 2, function(x){max(x, na.rm=T)}))
	
	if (bandorder=='BIL') {
		values <- as.vector(object@data@values)
	} else 	if (bandorder=='BIP') {
		values <- as.vector(t(object@data@values))
	}
	if (object@file@dtype == "INT" || object@file@dtype =='LOG' ) { 
		values <- as.integer(round(object@data@values))  
		values[is.na(values)] <- as.integer(object@file@nodatavalue)		
	} else { 
		values  <- as.numeric( object@data@values ) 
	}
	
	writeBin(values, object@file@con, size=object@file@dsize )

	if (dataIndices(object)[2] >= ncell(object)) {
		object <- .stopBrickRowWriting(object, ...)
		if (dataIndices(object)[2] > ncell(object)) {
			warning(paste('You have written beyond the end of file. last cell:', dataIndices(object)[2], '>', ncell(object)))
		}
		return(brick(filename(object)))
	}
	return(object)
}

