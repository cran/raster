# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric('writeStart')) {
	setGeneric('writeStart', function(x, filename, ...)
		standardGeneric('writeStart'))
}

if (!isGeneric('writeStop')) {
	setGeneric('writeStop', function(x)
		standardGeneric('writeStop'))
}
	
if (!isGeneric('writeValues')) {
	setGeneric('writeValues', function(x, v, start)
		standardGeneric('writeValues')) 
}


setMethod('writeStart', signature(x='RasterLayer', filename='character'), 
function(x, filename, options=NULL, format, ...) {

	filename <- .fullFilename(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	if (filetype=='ascii') { 
		x <- .startAsciiWriting(x, filename, ...)
	} else if ( filetype %in% .nativeDrivers() ) { 
		x <- .startRasterWriting(x, filename, format=filetype, ...)
	} else if ( filetype == 'CDF' ) { 
		x <- .startWriteCDF(x, filename, ...)
	} else {
		x <- .startGDALwriting(x, filename, options=options, format=filetype, ...)
	}		
	return(x)
})


setMethod('writeStart', signature(x='RasterBrick', filename='character'), 
function(x, filename, options=NULL, format, ...) {

	filename <- .fullFilename(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	
	if (filetype=='ascii') { stop('ascii files cannot write multi-layer files') }
	native <- filetype %in% c(.nativeDrivers(), 'ascii')
	if (native) { 
		x <- .startRasterWriting(x, filename, format=filetype, ...) 
	} else if ( filetype == 'CDF' ) { 
		x <- .startWriteCDF(x, filename, ...)
	} else {
		x <- .startGDALwriting(x, filename, options=options, format=filetype, ...) 
	}
	return(x)
})


setMethod('writeStop', signature(x='RasterLayer'), 
function(x) {
	if ( x@file@driver %in% .nativeDrivers() ) { 
		return( .stopRasterWriting(x) )
	} else if ( x@file@driver == 'ascii' ) { 
		return( .stopAsciiWriting(x) )
	} else if ( x@file@driver == 'netcdf' ) { 
		return( .stopWriteCDF(x) )
	} else {
		return( .stopGDALwriting(x) )
	}
})

setMethod('writeStop', signature(x='RasterBrick'), 
function(x) {
	native <- x@file@driver %in% c(.nativeDrivers())
	if (native) { 
		return( .stopRasterWriting(x) )
	} else if ( x@file@driver == 'netcdf' ) { 
		return( .stopWriteCDF(x) )
	} else {
		return( .stopGDALwriting(x) )
	}
})

