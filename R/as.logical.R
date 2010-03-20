# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: November 2009
# Version 0.9
# Licence GPL v3


setMethod('as.logical', signature(x='RasterLayer'), 
function(x, filename='', ...) {
	if (canProcessInMemory(x, 2)){
		
		x <- setValues(x, as.logical(getValues(x)))
		if (filename != '') {
			x <- writeRaster(x, filename, datatype='INT2S', ...)
		}
		return(x)
		
	} else {
		if (filename == '') {
			filename <- rasterTmpFile()					
		}
		out <- raster(x)
		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n, type=.progress(...))	
		for (i in 1:tr$n) {
			v <- as.logical ( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i] ) )
			writeValues(out, v, tr$row[i])
			pbStep(pb, i) 
		} 
		pbClose(pb)			
		out <- writeStop(out)		
		return(out)
	}
}
)
