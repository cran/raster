# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.makeTextFun <- function(fun) {
	if (class(fun) != 'character') {
		if (is.primitive(fun)) {
			test <- try(deparse(fun)[[1]], silent=TRUE)
			if (test == '.Primitive(\"sum\")') { fun <- 'sum' 
			} else if (test == '.Primitive(\"min\")') { fun <- 'min' 
			} else if (test == '.Primitive(\"max\")') { fun <- 'max' 
			}
		} else {
			test <- try(slot(fun, 'generic') == 'mean', silent=TRUE)
			if (isTRUE(test)) { fun <- 'mean' }
		} 
	}
	return(fun)
}


.getRowFun <- function(fun) {
	if (fun == 'mean') { return(rowMeans)
	} else if (fun == 'sum') { return(rowSums)
	} else if (fun == 'min') { return(.rowMin)
	} else if (fun == 'max') { return(.rowMax)
	} else { stop('unknown fun') }
}


setMethod('calc', signature(x='RasterStackBrick', fun='function'), 
function(x, fun, filename='', na.rm=TRUE, ...) {

	nl <- nlayers(x)
	if (nl == 1) { 	makemat <- TRUE	} else { makemat <- FALSE  }
	
	test <- length(fun(1:nl))
	if (test != 1) {
		if (test == nl) {
			return( .calcLayers(x, fun, filename, ...) )
		} else {
			stop("'fun' does not return the correct number of values. It should be 1 or nlayers(x)") 
		}
	}
	test <- try(fun(1:nl, na.rm=TRUE), silent=TRUE)
	if (class(test) == 'try-error') {
		stop("'fun' does take an 'na.rm' arugment. Add 'na.rm' or '...' to the function arguments") 
	}
	
	filename <- trim(filename)
	outraster <- raster(x)

	fun <- .makeTextFun(fun)
	if (class(fun) == 'character') { 
		rowcalc <- TRUE 
		fun <- .getRowFun(fun)
	} else { rowcalc <- FALSE }
	
	if (canProcessInMemory(x, 2)) {
		x <- getValues(x)
		if (makemat) { x <- matrix(x, ncol=1) }
		
		if (rowcalc) { 
			x <- fun(x, na.rm=na.rm ) #suggested by Matteo Mattiuzzi
		} else {
			x <- apply(x, 1, fun, na.rm=na.rm)
		}
		x <- setValues(outraster, x)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return ( x)		
	} 

# else 
	
	if (filename == '') { filename <- rasterTmpFile()	} 
	
	outraster <- writeStart(outraster, filename=filename, ...)
	tr <- blockSize(outraster)
	pb <- pbCreate(tr$n, type=.progress(...))			

	for (i in 1:tr$n) {
		v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		if (makemat) { v <- matrix(v, ncol=1) }
		if (rowcalc) {
			v <- fun(v, na.rm=na.rm)
		} else {
			v <- apply(v, 1, fun, na.rm=na.rm)
		}
		outraster <- writeValues(outraster, v, tr$row[i])
		pbStep(pb) 
	}
	outraster <- writeStop(outraster)
	pbClose(pb)
	return(outraster)
}
)

