# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, filename, ...)
		standardGeneric('writeRaster')) 
}


setMethod('writeRaster', signature(x='RasterLayer', filename='character'), 
function(x, filename, format, ...) {

	filename <- .fullFilename(filename)
	if (filename == '') {
		stop('provide a filename')
	}
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	
	if (! inMemory(x) ) {
		if (! fromDisk(x) ) {
			stop('No usable data available for writing')			
		} else {	
			if ( toupper(x@file@name) == toupper(filename) ) {
				stop('filenames of source and target should be different')
			}
			r <- raster(x)
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress(...))			
			r <- writeStart(r, filename=filename, format=filetype, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 			
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)
		}
	}

	if (.isNativeDriver(filetype)) {
		x <- .writeRasterAll(x, filename=filename, format=filetype, ...)
		
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, filename=filename,...)
		
	} else if (filetype=='CDF') {
		x <- .startWriteCDF(x, filename=filename, ...)
		x <- .writeValuesCDF(x, getValues(x))
		return( .stopWriteCDF(x) )
		
	} else { 
		x <- .writeGDALall(x, filename=filename, format=filetype, ...)
		
	}
	return(x)
}	
)


setMethod('writeRaster', signature(x='RasterStackBrick', filename='character'), 
function(x, filename, bandorder='BIL', format, ...) {

	filename <- .fullFilename(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)

	if (.isNativeDriver(filetype)) {
		return( .writeBrick(object=x, filename=filename, bandorder=bandorder, format=filetype, ...) )
	}

	if ( inMemory(x) ) {
	
		if (filetype=='CDF') {
			x <- .startWriteCDF(x, filename=filename,  ...)
			x <- .writeValuesBrickCDF(x, getValues(x) )	
			return( .stopWriteCDF(x) )
		} else {
			return ( .writeGDALall(x, filename=filename, format=filetype, ...) )
		}
		
	} else {
		if ( fromDisk(x) ) {
			
			if ( toupper(x@file@name) == toupper(filename) ) {
				stop('filenames of source and destination should be different')
			}
		
			b <- brick(x, values=FALSE)
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, type=.progress(...))
			b <- writeStart(b, filename=filename, bandorder=bandorder, format=filetype, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$size)
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			pbClose(pb)
			return(b)
			
		} else {
		
			stop('No cell values available for writing.')
		}
	}
}
)



.writeBrick <- function(object, filename, bandorder='BIL', format='raster', ...) {

	if (! format %in% c("raster", "BIL", "BSQ", "BIP") ) {
		stop('format should be one of "raster", "BIL", "BSQ", "BIP"')
	}
	if ( format %in% c("BIL", "BSQ", "BIP") ) { bandorder <- format }
	if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) { stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'") }
	
	out <- brick(object, values=FALSE)
	nl <- out@data@nlayers

	out <- writeStart(out, filename, bandorder=bandorder, ...)
	
	if (inMemory(object)) {
		out <- writeValues(out, getValues(object), 1)
	} else {
		tr <- blockSize(object)
		pb <- pbCreate(tr$n, type=.progress(...))
		for (i in 1:tr$n) {
			out <- writeValues(out, getValues(object, tr$row[i], tr$nrows[i]), tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
	}
	
	out <- writeStop(out)
	return( out )
}

