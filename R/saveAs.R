# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("saveAs")) {
	setGeneric("saveAs", function(x, filename, ...)
		standardGeneric("saveAs"))
}	



setMethod('saveAs', signature(x='RasterLayer', filename='character'), 
function(x, filename, ...) {
	filename <- trim(filename)
	if ( trim(filename(x)) == filename ) {
		stop('filenames of source and destination should be different')
	}
	
	ft = .filetype(...)
	if (ft == 'ascii') {
		return(.saveAsAscii(x, filename, ...))
	} else if (ft == 'raster') {
		fn = filename
		ext(fn) = '.grd'
		if ( trim(filename(x)) == fn ) {
			stop('filenames of source and destination should be different')
		}
	}
	
	
# to do also chec 	
	if (dataContent(x) == 'all') {
		return(  writeRaster(x, filename=filename, ...) )
	} 
# to do: if filetype and datatype are the same, then just copy the file .... 

	r <- raster(x)
	tr <- blockSize(r)
	pb <- pbCreate(tr$n, type=.progress(...))			
	r <- writeStart(r, filename=filename, ...)
	for (i in 1:tr$n) {
		v <- getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i])
		writeValues(r, v, tr$row[i])
		pbStep(pb, i) 			
	}
	r <- writeStop(r)
	pbClose(pb)
	return(r)
}
)



setMethod('saveAs', signature(x='RasterStackBrick', filename='character'), 
	function(x, filename, bandorder='BIL', ...) {
		filename <- trim(filename)
		if ( toupper(x@file@name == toupper(filename) )) {
			stop('filenames of source and destination should be different')
		}
		filetype <- .filetype(...)
		
		if (filetype=='raster') {
			return( .writeBrick(object=x, filename=filename, bandorder=bandorder, ...) )
		} else {
			b <- brick(x)
			for (r in 1:nrow(x)) {
				v <- getValues(x, r)
				b <- setValues(b, v)
				b <- .writeGDALrow(b, filename=filename, ...)
			}	
			return(b)
		}
	}
)



.saveAsAscii <- function(x, filename, filetype='ascii',...) {

	filename <- trim(filename)
	if ( trim(filename(x)) == filename ) {
		stop('filenames of source and destination should be different')
	}
	
	if (dataContent(x) == 'all') {
		return(  writeRaster(x, filename=filename, ...) )
	} 
# to do: if filetype and datatype are the same, then just copy the file .... 
	newr <- raster(x)
	for (r in 1:nrow(newr)) {
		x <- readRow(x, r)
		newr <- setValues(newr, values(x), r)
		newr <- writeRaster(newr, filename=filename, ..., doPB=TRUE)
	}
	return(newr)
}
