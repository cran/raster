# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('calc', signature(x='RasterStackBrick', fun='function'), 

function(x, fun, filename='', ...) {

	nl <- nlayers(x)
	test <- length(fun(1:nl))
	if (test != 1) {
		if (test == nl) {
			return( .calcLayers(x, fun, filename, ...) )
		} else {
			stop("'fun' does not return the correct number of values. It should be 1 or nlayers(x)") 
		}
	}

	filename <- trim(filename)
	outraster <- raster(x)

	if (!canProcessInMemory(x, 2) & filename == '') {
		filename <- rasterTmpFile()
	} 
	
	if (filename == '') {
		v <- matrix(NA, nrow=ncol(outraster), ncol=nrow(outraster))
	} else {
		outraster <- writeStart(outraster, filename=filename, ...)
	}
	tr <- blockSize(outraster)
	pb <- pbCreate(tr$n, type=.progress(...))			

	for (i in 1:tr$n) {
		
		sv <- apply(getValues(x, row=tr$row[i], nrows=tr$nrows[i]) ,  1,  fun)
		if (filename == "") {
			v[, tr$row[i]:(tr$row[i]+tr$nrows[i]-1)] <- matrix(sv, nrow=ncol(outraster))
		} else {
			outraster <- writeValues(outraster, sv, tr$row[i])
		}
		pbStep(pb) 
	}

	if (filename == "") { 	
		outraster <- setValues(outraster, as.vector(v))		
	} else {
		outraster <- writeStop(outraster)
	}

	pbClose(pb)
	return(outraster)
}
)

