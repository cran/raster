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

	stopifnot(hasValues(x))
	filename <- trim(filename)
	if (filename == '') {	stop('provide a filename')	}
	filename <- .fullFilename(filename)
	filetype <- .filetype(format , filename=filename)
	filename <- .getExtension(filename, filetype)
	
	if (filetype == 'KML') {
		KML(x, filename, ...) 
		return(invisible(x))
	}
	
	
	if (! inMemory(x) ) {
		if ( toupper(x@file@name) == toupper(filename) ) {
			stop('filenames of source and target should be different')
		}
		r <- raster(x)
		tr <- blockSize(r)
		pb <- pbCreate(tr$n, ...)			
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

	if (.isNativeDriver(filetype)) {
		x <- .writeRasterAll(x, filename=filename, filetype=filetype, ...)
		
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, filename=filename,...)
		
	} else if (filetype=='CDF') {
		x <- .startWriteCDF(x, filename=filename, ...)
		x <- .writeValuesCDF(x, getValues(x))
		return( .stopWriteCDF(x) )
		
	} else { 
		x <- .writeGDALall(x, filename=filename, ...)
		
	}
	return(x)
}	
)




setMethod('writeRaster', signature(x='RasterStackBrick', filename='character'), 
function(x, filename, format, ...) {

	stopifnot(hasValues(x))
	filename <- trim(filename)
	if (filename == '') {	stop('provide a filename')	}
	filename <- .fullFilename(filename)
	filetype <- .filetype(format, filename=filename)
	filename <- .getExtension(filename, filetype)

	if (filetype == 'KML') {
		KML(x, filename, ...) 
		return(invisible(x))
	}
	
	
	if (.isNativeDriver(filetype)) {
		if (! filetype %in% c("raster", "BIL", "BSQ", "BIP") ) {
			stop('this file format is not supported for multi-band files')
		}
	
		out <- brick(x, values=FALSE)
		layerNames(out) <- layerNames(x)
		out <- writeStart(out, filename, format=filetype, ...)
	
		if (inMemory(x)) {
			out <- writeValues(out, getValues(x), 1)
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, ...)
			for (i in 1:tr$n) {
				out <- writeValues(out, getValues(x, tr$row[i], tr$nrows[i]), tr$row[i])
				pbStep(pb, i)
			}
			pbClose(pb)
		}
		out <- .stopRasterWriting(out)
		return( out )
	}  
	
	# else 

	if ( inMemory(x) ) {
	
		if (filetype=='CDF') {
			b <- brick(x, values=FALSE)
			b@zvalue <- x@zvalue
			b@zname  <- x@zname
			b@z  <- x@z
			b <- .startWriteCDF(b, filename=filename,  ...)
			x <- .writeValuesBrickCDF(b, values(x) )	
			x <- .stopWriteCDF(x) 
		} else {
			x <- .writeGDALall(x, filename=filename, ...) 
		}
		
		return(x)
		
	} else {
			
		if ( toupper(filename(x)) == toupper(filename) ) {
			stop('filenames of source and destination should be different')
		}
		
		b <- brick(x, values=FALSE)
		
		if (filetype=='CDF') {
			b@zvalue <- x@zvalue
			b@zname  <- x@zname
			b@z  <- x@z
		}

		tr <- blockSize(b)
		pb <- pbCreate(tr$n, ...)
		b <- writeStart(b, filename=filename, format=filetype, ...)
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			b <- writeValues(b, v, tr$row[i])
			pbStep(pb, i)
		}
		b <- writeStop(b)
		pbClose(pb)
		return(b)
			
	} 
}
)

