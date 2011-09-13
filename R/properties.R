# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3


.driver <- function(object, warn=TRUE) {
	if (inherits(object, 'RasterStack')) {
		if (warn) {
			warning('There is no driver associated with a RasterStack')
		}
		return('')
	}
	d <- object@file@driver
	if (d == '' & warn) {
		warning('no file/driver associated with this object')
	} 
	return(d)
}



.nodatavalue <- function(object) {
	if (inherits(object, 'RasterStack')) {
		stop("no such thing exist for an entire 'RasterStack'")
	}
	return(object@file@nodatavalue)
}	


	
#	fileext <- toupper(extension(fn)) 
#	if ( fileext == ".GRD" | fileext == ".GRI" ) {
#		return('raster')
#	} else {
#		return('gdal')
#	}

#	fcon <- class(try( object@file@con, silent = T ))[1]
#	if (fcon == 'file') {
#		return('raster')
#	} else if (fcon == "GDALReadOnlyDataset") {
#		return('gdal')
#	} else if (fcon == "try-error") {
#		return('NA')
#	} else {
#		stop('unknown driver')
#	}

	

