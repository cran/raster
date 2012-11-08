# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.startRasterWriting <- function(x, filename, NAflag, update=FALSE, ...) {
 	filename <- trim(filename)
	if (filename == "") {
		stop('missing filename')
	}
	filetype <- .filetype(filename=filename, ...)
		
	filename <- .setFileExtensionHeader(filename, filetype)
	fnamevals <- .setFileExtensionValues(filename, filetype)
	datatype <- .datatype(...)
	
	dataType(x) <- datatype
	if (!missing(NAflag)) {
		x@file@nodatavalue <- NAflag
	}
	
	if (filetype %in% c('SAGA')) {
		resdif <- abs((yres(x) - xres(x)) / yres(x) )
		if (resdif > 0.01) {
			stop(paste("x has unequal horizontal and vertical resolutions. Such data cannot be stored in SAGA format"))
		}
	}

	overwrite <- .overwrite( ...)
	if (filetype == 'raster') {
		if (!overwrite & file.exists(filename)) {
			stop(paste(filename,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
		}
	} else {
		if (!overwrite & (file.exists(filename) | file.exists(fnamevals))) {
			stop(paste(filename,"or", fnamevals, "exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
		}
	}
	
	if (update) {
		attr(x@file, "con") <- file(fnamevals, "r+b")
	} else {
		attr(x@file, "con") <- file(fnamevals, "wb")
	}
	attr(x@file, "dsize") <- dataSize(x@file@datanotation)
	attr(x@file, "dtype") <- .shortDataType(x@file@datanotation)
	
	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))
	x@data@haveminmax <- FALSE
	x@file@driver <- filetype
	x@file@name <- filename

	

	if ( filetype %in% c("BIL", "BSQ", "BIP") ) { 
		bandorder <- filetype
	} else {
		bandorder <- 'BIL'
		if (nlayers(x) > 1) {
			bo <- list(...)$bandorder
			if (! is.null(bo)) {
				if (! bo %in% c('BIL', 'BIP', 'BSQ')) {
					warning('bandorder must be one of "BIL", "BSQ", or "BIP". Set to "BIL"')
				} else {
					bandorder <- bo
				}
			}
		}
	}
	x@file@bandorder <- bandorder
	
	return(x)
}



.stopRasterWriting <- function(x) {
	close(x@file@con)
#	fnamevals <- .setFileExtensionValues(x@file@name)
#	attr(x@file, "con") <- file(fnamevals, "rb")

	x@data@haveminmax <- TRUE
	if (x@file@dtype == "INT") {
		x@data@min <- round(x@data@min)
		x@data@max <- round(x@data@max)
	} else if ( x@file@dtype =='LOG' ) { 
#		x@data@min <- as.logical(x@data@min)
#		x@data@max <- as.logical(x@data@max)
	}
	
	#x@data@min[!is.finite(x@data@min)] <- NA
	#x@data@max[!is.finite(x@data@max)] <- NA
	
	hdr(x, .driver(x)) 
	filename <- .setFileExtensionValues(filename(x), x@file@driver)
	
	if (inherits(x, 'RasterBrick')) {
		r <- brick(filename, native=TRUE)
	} else {
		r <- raster(filename, native=TRUE)
	}
	
	if (! r@data@haveminmax) {
		r@data@min <- x@data@min
		r@data@max <- x@data@max
		r@data@haveminmax <- TRUE
	}
	return(r)
}		
 
 