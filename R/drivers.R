# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  November 2008
# Version 0.9
# Licence GPL v3


.nativeDrivers <- function() {
	return(  c("raster", "SAGA", "IDRISI", "BIL", "BSQ", "BIP") )
}

.nativeDriversLong <- function() {
	return(  c("R-raster", "SAGA GIS", "IDRISI", "Band by Line", "Band Sequential", "Band by Pixel") )
}


.isNativeDriver <- function(d) {
	return ( (d %in% .nativeDrivers() ) )
}

writeFormats <- function() {
	if ( .requireRgdal() ) {
		gd <- .gdalWriteFormats() 
		short <- c(.nativeDrivers(),  'ascii', as.vector(gd[,1]))
		long <- c(.nativeDriversLong(), 'Arc ASCII', as.vector(gd[,2]))
	} else {
		short <- c(.nativeDrivers(), 'ascii', 'CDF', "")
		long <- c(.nativeDriversLong(), "Arc ASCII", "netCDF / CF", "rgdal not installed")
	}
	
	m <- cbind(short, long)
	colnames(m) <- c("name", "long_name")
	return(m)
}

 