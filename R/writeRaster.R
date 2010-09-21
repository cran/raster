# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, filename, ...)
		standardGeneric('writeRaster')) 
}


setMethod('writeRaster', signature(x='RasterLayer', filename='character'), 
function(x, filename, format, ...) {

	filename <- trim(filename)
	if (filename == '') {
		stop('provide a filename')
	}
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	

	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			return( .saveAsRaster(x, filename, format=filetype, ...) )
		} else {
			stop('No usable data available for writing')
		}
	}

	if (.isNativeDriver(filetype)) {
		x <- .writeRasterAll(x, filename=filename, format=filetype, ...)
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, filename=filename,...)
	} else if (filetype=='CDF') {
		x <- .rasterSaveAsNetCDF(x, filename=filename, ...)
	} else { 
		x <- .writeGDALall(x, filename=filename, format=filetype, ...)
	}
	return(x)
}	
)


setMethod('writeRaster', signature(x='RasterBrick', filename='character'), 
function(x, filename, bandorder='BIL', format, ...) {

	filename <- trim(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			return( .saveAsBrick(x, filename, bandorder=bandorder, format=filetype, ...) )
		} else {
			stop('No usable data available for writing.')
		}
	}

	if (.isNativeDriver(filetype)) {
		return( .writeBrick(object=x, filename=filename, format=filetype, bandorder=bandorder, ...) )
	} else if (filetype=='CDF') {
		return ( .rasterSaveAsNetCDF(x, filename=filename, ...) )
	} else {
		return ( .writeGDALall(x, filename=filename, format=filetype, ...) )
	}
}
)


setMethod('writeRaster', signature(x='RasterStack', filename='character'), 
function(x, filename, bandorder='BIL', format, ...) {

	filename <- trim(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	if (filetype=='CDF') {
		# not memory safe
		x <- brick(x)
		return( .rasterSaveAsNetCDF(x, filename=filename, ...) )
	}
	b <- brick(x, values=FALSE)
	b <- writeStart(b, filename=filename, bandorder=bandorder, format=filetype, ...)
	tr <- blockSize(b)
	pb <- pbCreate(tr$n, type=.progress(...))
	for (i in 1:tr$n) {
		v <- getValues(x, row=tr$row[i], nrows=tr$size)
		b <- writeValues(b, v, tr$row[i])
		pbStep(pb, i)
	}
	pbClose(pb)
	b <- writeStop(b)
	return(invisible(b))
}
)

