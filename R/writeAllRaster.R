# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.writeRasterAll <- function(raster, filename, NAflag, filetype, ... ) {

	raster@file@driver <- filetype
 	filename <- trim(filename)
	fnamevals <- .setFileExtensionValues(filename, filetype)
	fnamehdr <- .setFileExtensionHeader(filename, filetype)
	if (filetype == 'raster') {
		filename <- fnamehdr
	} else {
		filename <- fnamevals
	}
	raster@file@name <- filename
	
	overwrite <- .overwrite(...)
	if (!overwrite & (file.exists(fnamehdr) | file.exists(fnamevals))) {
		stop(paste(filename,"exists. Use 'overwrite=TRUE' if you want to overwrite it")) 
	}
	
	raster@data@values[is.nan(raster@data@values)] <- NA
	raster@data@values[is.infinite(raster@data@values)] <- NA
	raster <- setMinMax(raster)

	datatype <- .datatype(...)
	dtype <- .shortDataType(datatype)
	dataType(raster) <- datatype
	
	mn <- minValue(raster)
	mx <- maxValue(raster)
	if (dtype == 'INT' ) {
		datatype <- .checkIntDataType(mn, mx, datatype)
		dataType(raster) <- datatype
		if (substr(datatype, 5 , 5) == 'U') { 
			raster@data@values[raster@data@values < 0] <- NA
			if (datatype == 'INT4U') { 
				raster@data@values[is.na(raster@data@values)] <- raster@file@nodatavalue
				i <- raster@data@values > 2147483647 & !is.na(raster@data@values)
				raster@data@values[i] <- 2147483647 - raster@data@values[i]
			} else {
				raster@data@values[is.na(raster@data@values)] <- raster@file@nodatavalue
			}
		} else {
			raster@data@values[is.na(raster@data@values)] <- raster@file@nodatavalue
		}
		raster@data@values <- as.integer(round(raster@data@values ))
		
	} else if ( dtype =='FLT') {
		raster@data@values <- as.numeric(raster@data@values)
	} else if ( dtype =='LOG') {
		raster@data@values <- as.integer(raster@data@values)
		raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)
	}
	

	if (! missing(NAflag) ) {
		raster@file@nodatavalue <- NAflag
	}
	dsize <- dataSize(raster@file@datanotation)
	filecon <- file(fnamevals, "wb")
	writeBin(raster@data@values , filecon, size = dsize ) 
	close(filecon)
	hdr(raster, filetype) 

	return(raster(filename, native=TRUE))
}
 
 