# Author: Robert J. Hijmans
# Date :  March 2012
# Version 1.0
# Licence GPL v3



#if (!isGeneric("focalApply")) {
#	setGeneric("focalApply", function(x, ngb, ...)
#		standardGeneric("focalApply"))
#}	


#setMethod("focalApply", signature(x='RasterLayer', ngb='numeric'), 
.focalApply <- function(x, ngb, fun, outside=NA, filename='', ...) {

	nl <- nlayers(x)
	ngb <- raster:::.checkngb(ngb, mustBeOdd=TRUE)
	out <- raster(x)
	dots <- list(...)
	na.rm <- dots$na.rm
	filename <- trim(filename)
	
	if (canProcessInMemory(x, prod(ngb)*2)) {
		x <- getValuesFocal(x, 1, nrow(out), ngb=ngb, outside=outside)
		if (!is.null(na.rm)) {
			x <- apply(x, 1, fun, na.rm=na.rm)
		} else {
			x <- apply(x, 1, fun)		
		}
		if (is.matrix(x)) {
			if (ncol(x) == ncell(out)) {
				x <- t(x)
			}
			if (ncol(x) > 1) {
				out <- brick(out, nl=ncol(x))
			}
		}
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		return(out)
	}

	out <- writeStart(out, filename=filename, ...)

	tr <- blockSize(out, n=prod(ngb)*2)
	pb <- pbCreate(tr$n, ...)
	
	if (nl == 1) {
		for (i in 1:tr$n) {
			x <- getValuesFocal(x, 1, nrow(out), ngb=ngb, outside=outside)
			if (!is.null(na.rm)) {
				x <- apply(x, 1, fun, na.rm=na.rm)
			} else {
				x <- apply(x, 1, fun)		
			}
			rotate <- FALSE
			if (is.matrix(x)) {
				if (i==1) {
					if (ncol(x) == ncell(out)) {
						rotate <- TRUE
						nlout <- nrow(x)
					} else {
						nlout <- ncol(x)
					} 
					if (nlout>1) {
						out <- brick(out, nl=nlout)
					}
				}
				if (rotate) {			
					x <- t(x)
				}
				out <- writeValues(out, x, tr$row[i])
				pbStep(pb, i)
			}
			pbClose(pb)			
		}
	} else {
		for (i in 1:tr$n) {
			x <- getValuesFocal(x, 1, nrow(out), ngb=ngb, outside=outside)
			if (!is.null(na.rm)) {
				x <- fun(x)
			} else {
				x <- fun(x)		
			}
			rotate <- FALSE
			if (is.matrix(x)) {
				if (i==1) {
					if (ncol(x) == ncell(out)) {
						rotate <- TRUE
						nlout <- nrow(x)
					} else {
						nlout <- ncol(x)
					} 
					if (nlout>1) {
						out <- brick(out, nl=nlout)
					}
				}
				if (rotate) {			
					x <- t(x)
				}
				out <- writeValues(out, x, tr$row[i])
				pbStep(pb, i)
			}
			pbClose(pb)			
		}
	}
		
	out <- writeStop(out)
	return(out)
} 
#)

	