# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("mask")) {
	setGeneric("mask", function(x, mask, ...)
		standardGeneric("mask"))
}	


setMethod('mask', signature(x='RasterLayer', mask='RasterLayer'), 
function(x, mask, filename="", ...){ 

	compare(x, mask)

	if (dataContent(x) == 'all' & dataContent(mask)=='all') {
		x[is.na(mask)] <- NA
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)
		
	} else if (canProcessInMemory(x, 3)) {
		if (dataContent(x) != 'all') { x <- readAll(x) }
		if (dataContent(mask) != 'all') { mask <- readAll(mask) }
		
		x[is.na(mask)] <- NA
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)
		
	} else {
		out <- raster(x)

		if (filename=='') { 	filename <- rasterTmpFile() }

		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out)

		pb <- pbCreate(tr$n, type=.progress(...))
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
			v[is.na(m)] <- NA
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		} 
		pbClose(pb)

		out <- writeStop(out)
		return(out)
	}
}
)


setMethod('mask', signature(x='RasterStack', mask='RasterLayer'), 
function(x, mask, filename="", ...){ 

	compare(x, mask)

	if (canProcessInMemory(mask, 2)) {
		# read mask only once..
		if (dataContent(mask) != 'all') { 
			mask <- readAll(mask) 
		}
	}
	
	for (i in 1:nlayers(x)) {
		x@layers[[i]] <- mask(x@layers[[i]], mask, filename='', ...)
	}
	
	return(x)
}
)



setMethod('mask', signature(x='RasterBrick', mask='RasterLayer'), 
function(x, mask, filename="", ...){ 

	compare(x, mask)
	
	if (canProcessInMemory(x, 3)) {
		if (dataContent(x) != 'all') { x <- readAll(x) }
		if (dataContent(mask) != 'all') { mask <- readAll(mask) }
	}
	if (dataContent(x) == 'all' & dataContent(mask) == 'all') {
		x@data@values[is.na(mask@data@values),] <- NA
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		} else {
			x@data@source <- 'ram'
			x@file@name <- ''
		}
		return(x)
	} else {
		out <- brick(x)
		if ( filename=='') { filename <- rasterTmpFile() }

		out <- writeStart(out, filename=filename, ...)

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, type=.progress(...))

		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
			v[is.na(m), ] <- NA
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		} 
		pbClose(pb)

		out <- writeStop(out)
		return(out)
	}
}
)

