# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3



.saveAsRaster <- function(x, filename, format, ...) {
#	filename <- trim(filename)
#	filetype <- .filetype(format=format, filename=filename)
#	filename <- .getExtension(filename, filetype)
	if ( toupper(x@file@name) == toupper(filename) ) {
# not entirely safe because ../this/that.tif could be the same as d:/this/that.tif
		stop('filenames of source and destination should be different')
	}

	r <- raster(x)
	tr <- blockSize(r)
	pb <- pbCreate(tr$n, type=.progress(...))			
	r <- writeStart(r, filename=filename, format=format, ...)
	for (i in 1:tr$n) {
		v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		r <- writeValues(r, v, tr$row[i])
		pbStep(pb, i) 			
	}
	r <- writeStop(r)
	pbClose(pb)
	return(r)
}




.saveAsBrick <- function(x, filename, bandorder='BIL', format, ...) {

		if ( toupper(x@file@name) == toupper(filename) ) {
			stop('filenames of source and destination should be different')
		}
		
		if (format=='raster') {
			return( .writeBrick(object=x, filename=filename, bandorder=bandorder, format=format, ...)) 
			
		} else {
			b <- brick(x, values=FALSE)
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, type=.progress(...))
			b <- writeStart(b, filename=filename, bandorder=bandorder, format=format, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$size)
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			pbClose(pb)
			return(b)
		}
	}



