# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2010
# Version 1.0
# Licence GPL v3


# this version of calc returns a RasterBrick with the same number of layers as x,
# base calc returns a RasterLayer

.calcLayers <- function(x, fun, filename='', ...) {

	filename <- trim(filename)
	outraster <- brick(x, values=FALSE)

	if ( canProcessInMemory(x, 2) ) {
		v <- t( fun( t( getValues(x) )) )
		if (filename != '') {
			outraster <- writeRaster(outraster)
		}
		return( setValues(outraster, v)	)
	}
	
	if ( filename == '') {
		filename <- rasterTmpFile()
	} 
	
	outraster <- writeStart(outraster, filename=filename, ...)
	tr <- blockSize(outraster)
	pb <- pbCreate(tr$n, type=.progress(...))			
	for (i in 1:tr$n) {
		sv <- t( fun( t( getValues(x, row=tr$row[i], nrows=tr$nrows[i]) )) )
		outraster <- writeValues(outraster, sv, tr$row[i])
		pbStep(pb) 
	}
	outraster <- writeStop(outraster)
	pbClose(pb)
	return(outraster)
}


