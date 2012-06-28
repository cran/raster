# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.writeRasterAll <- function(x, filename, NAflag, filetype, ... ) {

	x@file@driver <- filetype
 	filename <- trim(filename)
	fnamevals <- .setFileExtensionValues(filename, filetype)
	fnamehdr <- .setFileExtensionHeader(filename, filetype)
	if (filetype == 'raster') {
		filename <- fnamehdr
	} else {
		filename <- fnamevals
	}
	x@file@name <- filename
	
	overwrite <- .overwrite(...)
	if (!overwrite & (file.exists(fnamehdr) | file.exists(fnamevals))) {
		stop(paste(filename,"exists. Use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	
	na <- is.nan(x@data@values) | is.infinite(x@data@values)
	if (any(na)) {
		x@data@values[na] <- NA
	}
	x <- setMinMax(x)

	datatype <- .datatype(...)
	dtype <- .shortDataType(datatype)
	dataType(x) <- datatype
	
	mn <- minValue(x)
	mx <- maxValue(x)
	if (dtype == 'INT' ) {
		datatype <- .checkIntDataType(mn, mx, datatype)
		dataType(x) <- datatype
		if (substr(datatype, 5 , 5) == 'U') { 
			x@data@values[x@data@values < 0] <- NA
			if (datatype == 'INT4U') { 
				x@data@values[is.na(x@data@values)] <- x@file@nodatavalue
				i <- x@data@values > 2147483647 & !is.na(x@data@values)
				x@data@values[i] <- 2147483647 - x@data@values[i]
			} else {
				x@data@values[is.na(x@data@values)] <- x@file@nodatavalue
			}
		} else {
			x@data@values[is.na(x@data@values)] <- x@file@nodatavalue
		}
		x@data@values <- as.integer(round(x@data@values ))
		x@data@min <- round(x@data@min)
		x@data@max <- round(x@data@max)
		
	} else if ( dtype =='FLT') {
		x@data@values <- as.numeric(x@data@values)
	} else if ( dtype =='LOG') {
		x@data@values <- as.integer(x@data@values)
		x@data@values[is.na(x@data@values)] <- as.integer(x@file@nodatavalue)
	}
	

	if (! missing(NAflag) ) {
		x@file@nodatavalue <- NAflag
	}
	dsize <- dataSize(x@file@datanotation)
	filecon <- file(fnamevals, "wb")
	writeBin(x@data@values , filecon, size = dsize ) 
	close(filecon)
	
	hdr(x, filetype) 

	return(raster(filename, native=TRUE))
}
 
 