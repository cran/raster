# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  August 2010
# Version 1
# Licence GPL v3


stackApply <- function(x, indices, fun, filename='', na.rm=TRUE, ...) {

	nl <- nlayers(x)
	if (nl == 1) { 	makemat <- TRUE	} else { makemat <- FALSE  }
	
	
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

	fun <- .makeTextFun(fun)
	if (class(fun) == 'character') { 
		rowcalc <- TRUE 
		fun <- .getRowFun(fun)
	} else { rowcalc <- FALSE  	}
	
	
	if (canProcessInMemory(out, nl+nlout)) {
		v <- matrix(NA, nrow=ncell(out), ncol=nlout)
		a <- getValues(x)
		if (makemat) { a < - matrix(a, ncol=1) }

		if (rowcalc) {
			for (j in uin) {
				k <- which(ind == j)
				v[, j] <- fun(a[ , k, drop=FALSE], na.rm=na.rm)
			}
		} else {
			for (j in uin) {
				k <- which(ind == j)
				v[, j] <- apply(a[ , k, drop=FALSE], 1, fun, na.rm=na.rm)
			}
		}
		out <- setValues(out, v)
		if (filename != "") {
			out <- writeRaster(out, filename=filename, ...)
		}
		return(out)
	}
	
	if (filename == '') { filename <- rasterTmpFile() } 
	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out, n=nl+nlout)
	pb <- pbCreate(tr$n, type=.progress(...))

	v <- matrix(nrow=tr$nrows[1] * out@ncols, ncol=nlout)
	for (i in 1:tr$n) {
		a <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
		if (makemat) { a < - matrix(a, ncol=1) }
		if (i == tr$n) { v <- matrix(nrow=tr$nrows[i] * out@ncols, ncol=nlout) }

		if (rowcalc) {
			for (j in uin) {
				k <- which(ind == j)
				v[,j] <- fun(a[,k,drop=FALSE], na.rm=na.rm)
			}
		} else {
			for (j in uin) {
				k <- which(ind == j)
				v[,j] <- apply(a[,k,drop=FALSE], 1, fun, na.rm=na.rm)
			}
		}
		out <- writeValues(out, v, tr$row[i])
		pbStep(pb) 
	}

	out <- writeStop(out)
	pbClose(pb)
	return(out)
}
	

