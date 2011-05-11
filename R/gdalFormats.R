# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.isSupportedFormat <- function(dname) {
	res <- dname %in% c(.nativeDrivers(), 'ascii')
	if (!res) { 
		res <- .isSupportedGDALFormat(dname) 
	} 
	return(res)
}


.gdalWriteFormats <- function() {
	gd <- gdalDrivers()
	gd <- as.matrix(subset(gd, gd[,3] == T))
	i <- which(gd[,1] %in% c('VRT', 'MEM', 'MFF', 'MFF2'))
	gd[-i,]
}


.isSupportedGDALFormat <- function(dname) {
	if (! .requireRgdal() ) { stop('rgdal not available') }
	gd <- .gdalWriteFormats()
	res <- dname %in% gd[,1]
	if (!res) { stop(paste(dname, "is not a supported file format. See writeFormats()" ) ) }
	return(res)
}


#.GDALDataTypes <- c('Unknown', 'Byte', 'UInt16', 'Int16', 'UInt32','Int32', 'Float32', 'Float64', '
# what are these?  CInt16', 'CInt32',   'CFloat32', 'CFloat64')	 "as in C"?
# this needs to get fancier; depending on object and the abilties of the drivers
.getGdalDType <- function(dtype, format='') {
	if (!(dtype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT1U', 'INT2U', 'INT4U', 'FLT4S', 'FLT8S'))) {
		stop('not a valid data type')
	}
	type <- .shortDataType(dtype)
	size <- dataSize(dtype) * 8


	if (format=='BMP' | format=='ADRG' | format=='IDA' | format=='SGI') {
		return('Byte')
	}
	if (format=='PNM') {
		if (size == 8) {
			return('Byte')
		} else {
			return('UInt16')
		}
	}
	if (format=='RMF') {
		if (type == 'FLT') {
			return('Float64')
		}
	}
	
	if (type == 'LOG') {
		return('Byte')
	}
	if (type == 'INT') { 
		type <- 'Int' 
		if (size == 64) {
			size <- 32
			warning('8 byte integer values not supported by rgdal, changed to 4 byte integer values')
		}
		if (! dataSigned(dtype) ) {
			if (size == 8) {
				return('Byte')
			} else {
				type <- paste('U', type, sep='')
			}
		}
	} else { 
		type <- 'Float' 
	}
	return(paste(type, size, sep=''))
}


.getRasterDType <- function(dtype) {
	if (!(dtype %in% c('Byte', 'UInt16', 'Int16', 'UInt32','Int32', 'Float32', 'Float64', 'CInt16', 'CInt32', 'CFloat32', 'CFloat64'))) {
		return ('FLT4S')
	} else if  (dtype == 'Byte') {
		return('INT1U')
	} else if  (dtype == 'UInt16') {
		return('INT2U')
	} else if  (dtype == 'Int16' | dtype == 'CInt16') {
		return('INT2S')
	} else if  (dtype == 'UInt32') {
		return('INT4U')
	} else if  (dtype == 'Int32' | dtype == 'CInt32') {
		return('INT4S')
	} else if  (dtype == 'Float32' | dtype == 'CFloat32' ) {
		return('FLT4')
	} else if  (dtype == 'Float64' | dtype == 'CFloat64' )  {
		return('FLT8')
	} else {
		return('FLT4')	
	}
}
	