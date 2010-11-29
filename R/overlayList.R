# Author: Robert J. Hijmans
# Contributors: Reinhard Krug
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.overlayList <- function(x, fun, filename="", ...){ 
	
	if (length(x) < 1) { stop('no Rasters') }
	compare(x)
	
	nl <- sapply(x, nlayers)
	un <- unique(nl)
	#if ( max( max(un) %% un ) > 0) {
	#	stop('number of layers does not match (and cannot be recycled): ', paste(un, collapse=', '))
	#} 

	filename <- trim(filename)
	nl <- sapply(x, nlayers)
	maxnl <- max(nl)
	if (maxnl == 1) {
		outraster <- raster(x[[1]])
	} else {
		outraster <- brick(x[[1]], values=FALSE)
		outraster@data@nlayers <- as.integer(maxnl)
	}

	testmat <- matrix(1:10, nrow=10, ncol=length(x)) 
	test1 <- try ( apply(testmat, 1, fun) , silent=TRUE )
	if (class(test1) != "try-error") {
		doapply <- TRUE
		if (NCOL(test1) > 1) {
			if (class(outraster) == 'RasterLayer') {
				outraster <- brick(outraster)
				outraster@data@nlayers <- ncol(test1)
			} else {
				stop('cannot use this formula (multi-layer objects and multiple responses)')
			}
		}
	} else {
		doapply <- FALSE
		testlst <- vector(length=length(x), mode='list')
		for (i in 1:length(testlst)) { testlst[[i]] <- 1:10 }
		test2 <- try ( do.call(fun, testlst), silent=TRUE )
		if (class(test2) == "try-error" | length(test2) != 10) {
			stop('cannot use this formula, it is not vectorized')
		} 
	}


	if ( canProcessInMemory(outraster, sum(nl)) ) {
		pb <- pbCreate(3, type=.progress(...))			
		pbStep(pb, 1)
		if (doapply) {
			valmat = matrix(nrow= ncell(outraster)*maxnl , ncol=length(x)) 
			for (i in 1:length(x)) {
				if (ncell(x[[i]] < nrow(valmat))) {
					valmat[,i] <- as.vector(getValues(x[[i]])) * rep(1, nrow(valmat))
				} else {
					valmat[,i] <- as.vector(getValues(x[[i]]))
				}
			}
			pbStep(pb, 2)

			vals <- apply(valmat, 1, fun)
			if (! is.null(dim(vals))) {
				vals <- t(vals)
			}
			if (maxnl > 1) {
				vals <- matrix(vals, ncol=maxnl)
			}
			
		} else {
			for (i in 1:length(x)) {
				x[[i]] <- as.vector(getValues(x[[i]]))
			}
			pbStep(pb, 2)
			vals <- do.call(fun, x)
			if (maxnl > 1) {
				vals <- matrix(vals, ncol=maxnl)
			}
		}
		
		outraster <- setValues(outraster, vals)
		if (filename != "") { 
			outraster <- writeRaster(outraster, filename=filename, ...) 
		}
		pbStep(pb, 3)
		pbClose(pb)
		return(outraster)
		
	} else {
	
		if (filename == "") {
			filename <- rasterTmpFile()
		} 
		outraster <- writeStart(outraster, filename=filename, ...)
		
		tr <- blockSize(outraster, n=length(x))
		pb <- pbCreate(tr$n, type=.progress(...))			

		if (doapply) { 
			valmat = matrix(nrow=tr$nrows[1]*ncol(outraster)*nlayers(outraster) , ncol=length(x)) 
			for (i in 1:tr$n) {
				if (i == tr$n) {
					valmat = matrix(nrow=tr$nrows[i]*ncol(outraster)*nlayers(outraster) , ncol=length(x))
				}
				for (j in 1:length(x)) {
					if (ncell(x[[i]] < nrow(valmat))) {
						valmat[,j] <- as.vector(getValues(x[[j]], row=tr$row[i], nrows=tr$size)) * rep(1, nrow(valmat))
					} else {
						valmat[,j] <- as.vector(getValues(x[[j]], row=tr$row[i], nrows=tr$size))
					}
				}	
				
				vv <- apply(valmat, 1, fun)
				if (! is.null(dim(vv))) {
					vals <- t(vv)
				}
				if (nlayers(outraster) > 1) {
					vv <- matrix(vv, ncol=nlayers(outraster))
				}
				outraster <- writeValues(outraster, vv, tr$row[i])
				pbStep(pb, i)
			}
			
		} else {
			vallist <- list()
			for (i in 1:tr$n) {
				for (j in 1:length(x)) {
					vallist[[j]] <- as.vector( getValues(x[[j]], row=tr$row[i], nrows=tr$size) )
				}	
				vv <- do.call(fun, vallist)
				if (maxnl > 1) {
					vv <- matrix(vv, ncol=maxnl)
				}
				outraster <- writeValues(outraster, vv, tr$row[i])
				pbStep(pb, i)
			}
		}
		pbClose(pb)
		outraster <- writeStop(outraster)
	} 
	return(outraster)
}

