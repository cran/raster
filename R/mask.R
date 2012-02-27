# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("mask")) {
	setGeneric("mask", function(x, mask, ...)
		standardGeneric("mask"))
}	


setMethod('mask', signature(x='Raster', mask='Spatial'), 
function(x, mask, filename="", inverse=FALSE, ...){ 
	if (inverse) {
		mask <- rasterize(mask, x, -1)
		mask(x, mask, filename=filename, ...)
	
	} else {
		if (nlayers(x) > 1) {
			mask <- rasterize(mask, x, -1)
			mask(x, mask, filename=filename, ...)
		} else {
			rasterize(mask, x, filename=filename, mask=TRUE, ...)
		}
	}
} )



setMethod('mask', signature(x='RasterLayer', mask='RasterLayer'), 
function(x, mask, filename="", inverse=FALSE, ...){ 

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
		
		if (inverse) {
			x[!is.na(mask)] <- NA
		} else {
			x[is.na(mask)] <- NA
		}
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)
		
	} else {
		out <- raster(x)

		if (filename=='') { 	filename <- rasterTmpFile() }

		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, ...)

		if (inverse) {
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
				v[!is.na(m)] <- NA
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			} 		
		} else {
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
				v[is.na(m)] <- NA
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			} 
		}
		pbClose(pb)

		out <- writeStop(out)
		return(out)
	}
}
)


setMethod('mask', signature(x='RasterStackBrick', mask='RasterLayer'), 
function(x, mask, filename="", inverse=FALSE, ...){ 

	compare(x, mask)
	
	out <- brick(x, values=FALSE)
	layerNames(out) <- layerNames(x)
	
	if (canProcessInMemory(x, nlayers(x)+4)) {

		x <- getValues(x)
		if (inverse) {
			x[!is.na(getValues(mask)), ] <- NA
		} else {
			x[is.na(getValues(mask)), ] <- NA
		}
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		} 
		return(out)
		
	} else {
	

		if ( filename=='') { filename <- rasterTmpFile() }

		out <- writeStart(out, filename=filename, ...)

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, ...)

		if (inverse) {
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
				v[!is.na(m), ] <- NA
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			} 
		} else {
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
				v[is.na(m), ] <- NA
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			} 
		}

		pbClose(pb)

		out <- writeStop(out)
		return(out)
	}
}
)


setMethod('mask', signature(x='RasterLayer', mask='RasterStackBrick'), 
function(x, mask, filename="", ...){ 

	compare(x, mask)
	
	out <- brick(mask, values=FALSE)
	
	if (canProcessInMemory(mask, nlayers(x)*2+2)) {

		x <- getValues(x)
		x <- matrix(rep(x, nlayers(out)), ncol=nlayers(out))
		x[is.na(getValues(mask))] <- NA
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		} 
		return(out)
		
	} else {
	

		if ( filename=='') { filename <- rasterTmpFile() }

		out <- writeStart(out, filename=filename, ...)

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, ...)

		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
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



setMethod('mask', signature(x='RasterStackBrick', mask='RasterStackBrick'), 
function(x, mask, filename="", ...){ 


	if ( nlayers(x) != nlayers(mask) ) {
		if (nlayers(x) == 1) {
			x <- raster(x)
			return(mask(x, mask))
		}
		if (nlayers(mask) == 1) {
			mask <- raster(mask)
			return(mask(x, mask))
		}
		stop('number of layers of x and mask must match')
	}
	
	compare(x, mask)
	out <- brick(x, values=FALSE)
	layerNames(out) <- layerNames(x)
	
	if (canProcessInMemory(x, nlayers(x)+4)) {

		x <- getValues(x)
		x[is.na(getValues(mask))] <- NA
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		} 
		return(out)
		
	} else {

		if ( filename=='') { filename <- rasterTmpFile() }

		out <- writeStart(out, filename=filename, ...)

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, ...)

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

