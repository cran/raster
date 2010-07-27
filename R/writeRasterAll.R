# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.writeRasterAll <- function(raster, filename, ... ) {

	filetype <- .filetype(...)
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
		stop(paste(filename,"exists.","use 'overwrite=TRUE' if you want to overwrite it")) 
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
		raster@data@values <- as.integer(round(raster@data@values ))
		raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)				
	} else if ( dtype =='FLT') {
		raster@data@values <- as.numeric(raster@data@values)
		if (mn < -3.4E38 | mx > 3.4E38) { dataType(raster) <- 'FLT8S'
		} else { dataType(raster) <- 'FLT4S'
		}	
	} else if ( dtype =='LOG') {
		raster@data@values <- as.integer(raster@data@values)
		raster@data@values[is.na(raster@data@values)] <- as.integer(raster@file@nodatavalue)
	}

	dsize <- dataSize(raster@file@datanotation)
	filecon <- file(fnamevals, "wb")
	writeBin(raster@data@values , filecon, size = dsize ) 
	close(filecon)
	writeRasterHdr(raster, filetype) 

	return(raster(filename, native=TRUE))
}
 
 