# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3


.writeBrick <- function(object, filename, bandorder='BIL', ...) {

	filename <- trim(filename)
	if (filename == '') {	stop('you must supply a filename') 	}
	if (!bandorder %in% c('BIL', 'BSQ', 'BIP')) { stop("invalid bandorder, should be 'BIL', 'BSQ' or 'BIP'") }
	
	nl <- object@data@nlayers
	rout <- clearValues(object)

	rout@file@bandorder <- bandorder
	
	rout <- writeStart(rout, filename, ...)
	if (bandorder=='BSQ') {
		if (class(object) != 'RasterStack') {
			if (dataContent(object) == 'all') {
				writeValues(rout, as.vector(object@data@values))
				v <- na.omit(object@data@values) 
				if (length(v) > 0) {
					rout@data@min <- apply(v, 2, min)
					rout@data@max <- apply(v, 2, max)
				}
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
				writeValues(rout, as.vector(v))
				v <- na.omit(v) 
				if (length(v) > 0) {
					rout@data@min[i] <- min(rout@data@min[i], v)
					rout@data@max[i] <- max(rout@data@max[i], v)
				}	
				rr <- rr + 1
				pbStep(pb, rr) 				
			}
		}				
		pbClose(pb)
	} else if (bandorder=='BIL') {
		pb <- pbCreate(nrow(rout), type=.progress(...))
		for (r in 1:nrow(object)) {
			v <- getValues(object, r)
			writeValues(rout, as.vector(v))
			v <- na.omit(v)
			if (length(v) > 0) {
				rout@data@min <- pmin(rout@data@min, apply(v, 2, min))
				rout@data@max <- pmax(rout@data@max, apply(v, 2, max))
			}
			pbStep(pb, r) 				
		}
		pbClose(pb)
	} else if (bandorder=='BIP') {
		pb <- pbCreate(nrow(rout), type=.progress(...))
		for (r in 1:nrow(object)) {
			v <- getValues(object, r)
			writeValues(rout, as.vector(t(v)))
			v <- na.omit(v) 
			if (length(v) > 0) {
				rout@data@min <- pmin(rout@data@min, apply(v, 2, min))
				rout@data@max <- pmax(rout@data@max, apply(v, 2, max))
			}
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

