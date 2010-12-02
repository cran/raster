# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  August 2010
# Version 1.0
# Licence GPL v3
# this version of calc returns a RasterBrick 
# base calc returns a RasterLayer

.calcLayers <- function(x, fun, filename='', ...) {

	filename <- trim(filename)
	out <- brick(x, values=FALSE)
	
	if ( canProcessInMemory(x, 2) ) {
		out <- setValues(out, t( fun( t( getValues(x) )) ) )
		if (filename != '') {
			out <- writeRaster(out, filename=filename, ...)
		}
		return(out)
	}
	
	if ( filename == '') {
		filename <- rasterTmpFile()
	} 
	
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, type=.progress(...))			
	out <- writeStart(out, filename=filename, ...)
	for (i in 1:tr$n) {
		v <- t( fun( t( getValues(x, row=tr$row[i], nrows=tr$nrows[i]) )) )
		out <- writeValues(out, v, tr$row[i])
		pbStep(pb) 
	}
	out <- writeStop(out)
	pbClose(pb)
	return(out)
}

