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
	filename <- raster:::.fullFilename(filename)
	filetype <- raster:::.filetype(format=format, filename=filename)
	filename <- raster:::.getExtension(filename, filetype)
	
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

	stopifnot(hasValues(x))
	filename <- trim(filename)
	if (filename == '') {	stop('provide a filename')	}
	filename <- .fullFilename(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)

	if (.isNativeDriver(filetype)) {
		if (! filetype %in% c("raster", "BIL", "BSQ", "BIP") ) {
			stop('file format not supported for multi-band files')
		}
		if ( filetype %in% c("BIL", "BSQ", "BIP") ) { 
			bandorder <- filetype
		}
		if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) { 
			stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'") 
		}

		out <- brick(x, values=FALSE)
		layerNames(out) <- layerNames(x)
		out <- writeStart(out, filename, bandorder=bandorder, ...)
	
		if (inMemory(x)) {
			out <- writeValues(out, getValues(x), 1)
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, type=.progress(...))
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
			b <- .startWriteCDF(b, filename=filename,  ...)
			x <- .writeValuesBrickCDF(b, getValues(x) )	
			x <- .stopWriteCDF(x) 
		} else {
			x <- .writeGDALall(x, filename=filename, format=filetype, ...) 
		}
		
		return(x)
		
	} else {
			
		if ( toupper(filename(x)) == toupper(filename) ) {
			stop('filenames of source and destination should be different')
		}
		
		b <- brick(x, values=FALSE)
		tr <- blockSize(b)
		pb <- pbCreate(tr$n, type=.progress(...))
		b <- writeStart(b, filename=filename, bandorder=bandorder, format=filetype, ...)
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

