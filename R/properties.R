# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3


.driver <- function(object) {
	fn <- filename(object)
	if (fn == '') {
		stop('no file asociated with this object')
	} else {
		return(object@file@driver)
	}
}



.nodatavalue <- function(object) {
	if (class(object) == 'RasterStack') {
		stop("no such thing exist for an entire 'RasterStack'")
	}
	return(object@file@nodatavalue)
}	


	
#	fileext <- toupper(ext(fn)) 
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

	

