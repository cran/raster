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

	return( calc(stack(x), fun, filename=filename, ...) )
	
	
	# ignored for now..
	
	test = try(fun(x[1]), silent=TRUE)
	if (class(test) == 'try-error') {
		stop("function 'fun' is not valid here")
	}
	if (length(test) > 1) { 
		out <- brick(x)
	} else {
		out <- raster(x)
	}	

	if (canProcessInMemory(x, 3) |  inMemory(x)  ) {
		x <- getValues(x)
		out <- setValues(out, fun(x)) 
		if (filename != "") {
			out <- writeRaster(out, filename=filename, ...)
		}
		return(out)
		
	} else if (filename == '') {
		filename <- rasterTmpFile()
	}
	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, type=.progress(...))			
	
	for (i in 1:tr$n) {
		vv <- fun( getValues(x, row=tr$row[i], nrows=tr$nrows[i]) )
		out <- writeValues(out, vv, tr$row[i])
		pbStep(pb, i)
	}
	pbClose(pb)
	out <- writeStop(out)
	
	return(out)
}
)
