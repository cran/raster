# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  August 2010
# Version 1
# Licence GPL v3


stackApply <- function(x, indices, fun, filename='', na.rm=TRUE, ...) {

	nl <- nlayers(x)
	if (nl == 1) { 	
		makemat <- TRUE	
	} else { 
		makemat <- FALSE  
	}
	
	
	ind <- vector(length=nl)
	# perhaps we need recycling:
	ind[] <- indices
	
	uin <- unique(ind)
	nlout <- length(uin)
	if (nlout > 1) {
		out <- brick(x, values=FALSE)
		out@data@nlayers <- nlout
	} else {
		out <- raster(x)
	}

	filename <- trim(filename)

	rowcalc <- FALSE
	fun <- raster:::.makeTextFun(fun)
	if (class(fun) == 'character') { 
		rowcalc <- TRUE 
		fun <- raster:::.getRowFun(fun)
	} 
	
	if (canProcessInMemory(out, nl+nlout)) {
		x <- getValues(x)
		if (makemat) { 
			x <- matrix(x, ncol=1) 
		}
		pb <- pbCreate(3,...)
		pbStep(pb)
		if (rowcalc) {
			v <- lapply(uin, function(i) fun(x[, ind==uin[i], drop=FALSE], na.rm=na.rm))
		} else {
			v <- lapply(uin, function(i, ...) apply(x[, ind==uin[i], drop=FALSE], 1, fun, na.rm=na.rm))
		}
		pbStep(pb)		
		v <- do.call(cbind, v)
		out <- setValues(out, v)
		if (filename != "") {
			out <- writeRaster(out, filename=filename, ...)
		}
		pbStep(pb)
		pbClose(pb)
		return(out)
	}

	if (filename == '') { filename <- rasterTmpFile() } 
	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out, n=nl+nlout)
	pb <- pbCreate(tr$n, ...)

	for (i in 1:tr$n) {
		a <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		if (makemat) { 
			a <- matrix(a, ncol=1) 
		}

		if (rowcalc) {
			v <- lapply(uin, function(i) fun(a[, ind==uin[i], drop=FALSE], na.rm=na.rm))
		} else {
			v <- lapply(uin, function(i, ...) apply(a[, ind==uin[i], drop=FALSE], 1, fun, na.rm=na.rm))
		}
		v <- do.call(cbind, v)
		out <- writeValues(out, v, tr$row[i])
		pbStep(pb) 
	}

	out <- writeStop(out)
	pbClose(pb)
	return(out)
}
	


