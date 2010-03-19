# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.writeSparse <- function(raster, filename, overwrite=FALSE) {

#	raster@file@driver <- 'raster'
	if (!overwrite & file.exists(filename)) {
		stop(filename, "exists. Use 'overwrite=TRUE' if you want to overwrite it") 
	}

	raster@data@values[is.nan(values(raster))] <- NA

	dtype <- .shortDataType(raster@data@datanotation)
	if (dtype == "integer") { 
		raster@data@values <- as.integer(values(raster)) 
	}
	if (class(values(raster))=='integer') {
		dataType(raster) <- 'INT4S'
	}	
	raster <- setMinMax(raster)

	binraster <- .setFileExtensionValues(raster@file@name, 'raster')

	raster <- openConnection(raster)
	writeBin( as.vector(dataIndices(raster)), raster@file@con, size = as.integer(4)) 
	writeBin( as.vector(values(raster)), raster@file@con, size = dataSize(raster@file@datanotation) ) 
	raster <- closeConnection(raster)

	# add the 'sparse' key word to the hdr file!!!
	writeRasterHdr(raster) 
	return(raster)
} 

