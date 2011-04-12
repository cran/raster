# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3



.calcFilter <- function(rows, colnrs, res, filter, fun) {
	res[] <- NA
    for (i in 1:dim(rows)[2]) {
		d <- rows[, colnrs[i, ]]
		if (!all(dim(d) == dim(filter))) {
			res[i] <- NA
		} else {
			res[i] <- fun(d * filter)
		}
	}	
	return(res)
}


focalFilter <- function(x, filter, fun=sum, filename="", ...) {
	if (!is.matrix(filter)) { stop('filter must be a matrix') }
	ngb <- dim(filter)
	if (prod(ngb) == 0) { stop('ncol and nrow of filter must be > 0') }

	ngbgrid <- raster(x)

	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(ngbgrid)+limcol)
	colnrs <- .embed(colnrs, ngb[2])
	colnrs[colnrs > ncol(ngbgrid) | colnrs < 0] <- 0

	limrow <- floor(ngb[1] / 2)
	ngbdata <- matrix(NA, nrow=0, ncol=ncol(ngbgrid))
# add all rows needed for first ngb, minus 1 that will be read in first loop	
	ngbdata <- getValues(x, 1, limrow)
	ngbdata <- matrix(ngbdata, nrow=limrow, byrow=TRUE)

	res <- vector(length=ncol(ngbdata))

	filename <- trim(filename)
	if (!canProcessInMemory(ngbgrid, 2) && filename == '') {
		filename <- rasterTmpFile()			
	}
	
	if (filename == '') {
		v <- matrix(NA, ncol=nrow(ngbgrid), nrow=ncol(ngbgrid))
	} else {
		v <- vector(length=0)
		ngbgrid <- writeStart(ngbgrid, filename=filename, ...)
	}

	pb <- pbCreate(nrow(ngbgrid), type=.progress(...))

	for (r in 1:nrow(ngbgrid)) {		
		rr <- r + limrow
		if (rr <= nrow(ngbgrid)) {
			rowdata <- getValues(x, rr)
			if (dim(ngbdata)[1] == ngb[1]) {
				ngbdata <- rbind(ngbdata[2:ngb[1],], rowdata)
			} else {
				ngbdata <- rbind(ngbdata, rowdata)			
			}
		} else {
			ngbdata <- ngbdata[-1, ,drop=FALSE]
		}

		
		ngbvals <- .calcFilter(ngbdata, colnrs, res, filter, fun)
		if (filename != "") {
			ngbgrid <- writeValues(ngbgrid, ngbvals, r)
		} else {
			v[,r] <- ngbvals
		}
		pbStep(pb, r)
	}
	pbClose(pb)
	
	if (filename == "") { 
		ngbgrid <- setValues(ngbgrid, as.vector(v)) 
	} else {
		ngbgrid <- writeStop(ngbgrid)
	}
	return(ngbgrid)
}
	
