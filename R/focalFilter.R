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


focalFilter <- function(raster, filter, fun=sum, filename="", ...) {
	if (!is.matrix(filter)) {stop('filter must be a matrix')}
	ngb <- dim(filter)
	if (prod(ngb) == 0) {stop('ncol and nrow of filter must be > 0') }

	ngbgrid <- raster(raster)

	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(ngbgrid)+limcol)
	colnrs <- .embed(colnrs, ngb[2])
	colnrs[colnrs > ncol(ngbgrid) | colnrs < 0] <- 0

	limrow <- floor(ngb[1] / 2)
	ngbdata <- matrix(NA, nrow=0, ncol=ncol(ngbgrid))
# add all rows needed for first ngb, minus 1 that will be read in first loop	
	for (r in 1:limrow) {
		if (dataContent(raster)=='all') {
			rowdata <- getValues(raster, r)
		} else {	
			rowdata <- values(readRow(raster, r))
		}
		ngbdata <- rbind(ngbdata, rowdata)
	}

	res <- vector(length=ncol(ngbdata))

	filename <- trim(filename)
	if (!canProcessInMemory(ngbgrid, 2) && filename == '') {
		filename <- rasterTmpFile()
								
	}
	
	if (filename == '') {
		v <- matrix(NA, ncol=nrow(ngbgrid), nrow=ncol(ngbgrid))
	} else {
		v <- vector(length=0)
	}

	
	pb <- pbCreate(nrow(ngbgrid), type=.progress(...))

	for (r in 1:nrow(ngbgrid)) {		
		rr <- r + limrow
		if (rr <= nrow(ngbgrid)) {
			if (dataContent(raster)=='all') {
				rowdata <- getValues(raster, rr)
			} else {	
				rowdata <- values(readRow(raster, rr))
			}
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
			ngbgrid <- setValues(ngbgrid, ngbvals, r)
			ngbgrid <- writeRaster(ngbgrid, filename=filename, ...)
		} else {
			v[,r] <- ngbvals
		}
		pbStep(pb, r)
	}
	pbClose(pb)
	
	if (filename == "") { 
		ngbgrid <- setValues(ngbgrid, as.vector(v)) 
	}
	return(ngbgrid)
}
	
