# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3


.writeBrick <- function(object, filename, bandorder='BIL', format='raster', ...) {

	filename <- trim(filename)
	if (filename == '') {	stop('you must supply a filename') 	}

	if (! format %in% c("raster", "BIL", "BSQ", "BIP") ) {
		stop('format should be one of "raster", "BIL", "BSQ", "BIP" (or a rgdal supported format)')
	}
	if ( format %in% c("BIL", "BSQ", "BIP") ) {
		bandorder <- format
	}
	if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) { 
		stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'") 
	}
	
	nl <- object@data@nlayers
	rout <- clearValues(object)

	rout@file@bandorder <- bandorder
	
	rout <- writeStart(rout, filename, ...)
	if (bandorder=='BSQ') {
		if (! inherits(object, 'RasterStack')) {
			if (inMemory(object)) {
				rout <- writeValues(rout, object@data@values)
				rout <- writeStop(rout)
				return(rout)
			}
		} 

		pb <- pbCreate(rout@nrows*nl, type=.progress(...))
		rr <- 0
		for (i in 1:nl) {
			sr <- raster(object, i)
			for (r in 1:sr@nrows) {
				v <- getValues(sr, r)
				rout <- writeValues(rout, v)
				rr <- rr + 1
				pbStep(pb, rr) 				
			}
		}				
		pbClose(pb)
		
	} else if (bandorder=='BIL') {
		pb <- pbCreate(nrow(rout), type=.progress(...))
		for (r in 1:nrow(object)) {
			v <- getValues(object, r)
			rout <- writeValues(rout, v)
			pbStep(pb, r) 				
		}
		pbClose(pb)
		
	} else if (bandorder=='BIP') {
		pb <- pbCreate(nrow(rout), type=.progress(...))
		for (r in 1:nrow(object)) {
			v <- getValues(object, r)
			rout <- writeValues(rout, t(v))
			pbStep(pb, r) 				
		}
		pbClose(pb)
	}
	rout <- writeStop(rout)

#	rout <- brick(filename(rout))
#	rout@data@min <- minValue(object, -1)
#	rout@data@max <- maxValue(object, -1)
#	rout@data@nlayers <- nl
#	rout@file@bandorder <- bandorder
#	rout@layernames <- layerNames(object)
#	writeRasterHdr(rout, format=.filetype(...))
	return(brick(filename))
}

