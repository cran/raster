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
	test = try(fun(1), silent=TRUE)
	if (class(test) == 'try-error') {
		stop("function 'fun' is not valid here")
	}
	if (length(fun(1)) > 1) { 
		stop("function 'fun' returns more than one value")
	}

	if (!(dataContent(x) == 'all' | dataSource(x) == 'disk')) {
		stop('RasterLayer has no data on disk, nor a complete set of values in memory')
	}

	filename <- trim(filename)
	outraster <- raster(x)

	if (dataSource(x) == 'disk') {
		if (!canProcessInMemory(x, 3) & filename == '') {
			filename <- rasterTmpFile()
		}
	}
	
	if ( dataContent(x) == 'all' ) {
		outraster <- setValues(outraster, fun(values(x))) 
		if (filename != "") {
			outraster <- writeRaster(outraster, filename=filename, ...)
		}
		return(outraster)
	} 
	
	if (filename == '') {
		v <- matrix(ncol=nrow(outraster), nrow=ncol(outraster))
	} else {
		outraster <- writeStart(outraster, filename=filename, ...)
	}
		
	tr <- blockSize(outraster)
	pb <- pbCreate(tr$n, type=.progress(...))			
	
	for (i in 1:tr$n) {
		vv <- fun( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]) )
		if (filename == "") {
			cols <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)	
			v[,cols] <- matrix(vv, nrow=outraster@ncols)
		} else {
			writeValues(outraster, vv, tr$row[i])
		}
		pbStep(pb, i)
	}
	pbClose(pb)
		
	if (filename == "") { 
		outraster <- setValues(outraster, as.vector(v)) 
	} else {
		outraster <- writeStop(outraster)
	}
	return(outraster)
}
)
