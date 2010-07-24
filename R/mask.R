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

	if ( inMemory(x) & inMemory(mask)=='all') {
		x[is.na(mask)] <- NA
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)
		
	} else if (canProcessInMemory(x, 3)) {
		if (! inMemory(x) ) { x <- readAll(x) }
		if (! inMemory(mask) ) { mask <- readAll(mask) }
		
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


setMethod('mask', signature(x='RasterStackBrick', mask='RasterLayer'), 
function(x, mask, filename="", ...){ 

	compare(x, mask)
	
	outRaster <- brick(x, values=FALSE)
	
	if (canProcessInMemory(x, nlayers(x)+4)) {

		x <- getValues(x)
		x[is.na(getValues(mask)), ] <- NA
		outRaster <- setValues(outRaster, x)
		if (filename != '') {
			outRaster <- writeRaster(outRaster, filename, ...)
		} 
		return(outRaster)
		
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

