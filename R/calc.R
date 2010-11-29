# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("calc")) {
	setGeneric("calc", function(x, fun, ...)
		standardGeneric("calc"))
}	

setMethod('calc', signature(x='RasterLayer', fun='function'), 

function(x, fun, filename='', ...) {

	if (!hasValues(x)) {
		stop('RasterLayer has no cell values in memory or on disk')
	}

	test = try(fun(x[1]), silent=TRUE)
	if (class(test) == 'try-error') {
		stop("function 'fun' is not valid here")
	}
	if (length(fun(1)) > 1) { 
		stop("function 'fun' returns more than one value")
	}


	filename <- trim(filename)
	outraster <- raster(x)

	if (canProcessInMemory(x, 3) |  inMemory(x)  ) {
		x <- getValues(x)
		outraster <- setValues(outraster, fun(x)) 
		if (filename != "") {
			outraster <- writeRaster(outraster, filename=filename, ...)
		}
		return(outraster)
		
	} else if (filename == '') {
		filename <- rasterTmpFile()
	}
	
	outraster <- writeStart(outraster, filename=filename, ...)
	tr <- blockSize(outraster)
	pb <- pbCreate(tr$n, type=.progress(...))			
	
	for (i in 1:tr$n) {
		vv <- fun( getValues(x, row=tr$row[i], nrows=tr$nrows[i]) )
		outraster <- writeValues(outraster, vv, tr$row[i])
		pbStep(pb, i)
	}
	pbClose(pb)
	outraster <- writeStop(outraster)
	
	return(outraster)
}
)
