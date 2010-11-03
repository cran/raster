# Author: Robert J. Hijmans & Matteo Mattiuzzi
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
function(x, fun, filename='', na.rm, ...) {

	nl <- nlayers(x)
	if (nl == 1) { 	makemat <- TRUE	} else { makemat <- FALSE  }
	
	tst <- fun(rbind(1:nl, 1:nl, 1:nl))
	test <- dim(tst)
	
	if (! is.null(test)) {
		if (test[1] == 3 & test[2] == nl) {
			return( .calcLayers(x, fun, filename, ...) )
		}
	} else {
		test <- length(tst)
	}
		#} else {
		#	stop("'fun' does not return the correct number of values. It should be 1 or nlayers(x)") 
		#}
	
	if (! missing(na.rm)) {
		test <- try(fun(1:nl, na.rm=TRUE), silent=TRUE)
		if (class(test) == 'try-error') {
			stop("'fun' does take an 'na.rm' arugment. Add 'na.rm' or '...' to the function arguments") 
		}
	}
	
	filename <- trim(filename)
	if (test == 1) {
		out <- raster(x)
	} else {
		out <- brick(x, values=FALSE)
		out@data@nlayers <- test
	}

	fun <- .makeTextFun(fun)
	if (class(fun) == 'character') { 
		rowcalc <- TRUE 
		fun <- .getRowFun(fun)
	} else { rowcalc <- FALSE }
	
	if (canProcessInMemory(x, 2)) {
		x <- getValues(x)
		if (makemat) { x <- matrix(x, ncol=1) }
		if (missing(na.rm)) {
			if (rowcalc) { 
				x <- fun(x ) 
			} else {
				x <- apply(x, 1, fun )
			}
		} else {
			if (rowcalc) { 
				x <- fun(x, na.rm=na.rm ) 
			} else {
				x <- apply(x, 1, fun, na.rm=na.rm)
			}
		}
		if (is.matrix(x)) {
			if (dim(x)[2] != test) {
				x <- t(x)
			}
		}
		x <- setValues(out, x)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return ( x)		
	}

# else 
	
	if (filename == '') { filename <- rasterTmpFile()	} 
	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, type=.progress(...))			

	if (missing(na.rm)) {
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if (makemat) { v <- matrix(v, ncol=1) }
			if (rowcalc) {
				v <- fun(v)
			} else {
				v <- apply(v, 1, fun)
			}
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb) 
		}
	} else {
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if (makemat) { v <- matrix(v, ncol=1) }
			if (rowcalc) {
				v <- fun(v, na.rm=na.rm)
			} else {
				v <- apply(v, 1, fun, na.rm=na.rm)
			}
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb) 
		}
	}
	out <- writeStop(out)
	pbClose(pb)
	return(out)
}
)

