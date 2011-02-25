# Author: Robert J. Hijmans
# Contributors: Reinhard Krug
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.overlayList <- function(x, fun, filename="", ...){ 
	
	if (length(x) < 1) { stop('no Rasters') }
	compare(x)
	
	nl <- sapply(x, nlayers)
	maxnl <- max(nl)

	filename <- trim(filename)

	testmat <- NULL
	testlst <- vector(length=length(x), mode='list')
	for (i in 1:length(testlst)) {
		v <- as.vector(extract(x[[i]], 1:5))
		testmat <- cbind(testmat, v)
		testlst[[i]] <- as.vector(v)
	}

	test1 <- try ( apply(testmat, 1, fun) , silent=TRUE )
	if (class(test1) != "try-error") {
		doapply <- TRUE
		if (! is.null(dim(test1))) {
			test1 <- t(test1)
		} else {
			test1 <- matrix(test1, ncol=maxnl)
		}
		nlout <- NCOL(test1)
	} else {
		doapply <- FALSE
		test2 <- try ( do.call(fun, testlst), silent=TRUE )
		nlout <- length(test2)/5
		if (class(test2) == "try-error" | length(test2) < 5) {
			stop('cannot use this formula, it is not vectorized')
		} 
	}

	if (nlout == 1) {
		outraster <- raster(x[[1]])
	} else {
		outraster <- brick(raster(x[[1]]))
	}
	
	if ( canProcessInMemory(outraster, sum(nl)) ) {
		pb <- pbCreate(3, type=.progress(...))			
		pbStep(pb, 1)
		if (doapply) {
			valmat <- matrix(nrow=ncell(outraster)*maxnl, ncol=length(x)) 
			for (i in 1:length(x)) {
				if (ncell(x[[i]]) < nrow(valmat)) {
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
			vals <- matrix(vals, nrow=ncell(outraster))
			
		} else {
			for (i in 1:length(x)) {
				x[[i]] <- as.vector(getValues(x[[i]]))
			}
			pbStep(pb, 2)
			vals <- do.call(fun, x)
			vals <- matrix(vals, nrow=ncell(outraster))
		}
		pbStep(pb, 3)
		outraster <- setValues(outraster, vals)
		if (filename != "") { 
			outraster <- writeRaster(outraster, filename=filename, ...) 
		}
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
			valmat = matrix(nrow=tr$nrows[1]*ncol(outraster)*maxnl, ncol=length(x)) 
			for (i in 1:tr$n) {
				if (i == tr$n) {
					valmat = matrix(nrow=tr$nrows[i]*ncol(outraster)*maxnl , ncol=length(x))
				}
				for (j in 1:length(x)) {
					if (ncell(x[[i]]) < nrow(valmat)) {
						valmat[,j] <- as.vector(getValues(x[[j]], row=tr$row[i], nrows=tr$size)) * rep(1, nrow(valmat))
					} else {
						valmat[,j] <- as.vector(getValues(x[[j]], row=tr$row[i], nrows=tr$size))
					}
				}	
				
				vv <- apply(valmat, 1, fun)
				if (! is.null(dim(vv))) {
					vals <- t(vv)
				}
				vv <- matrix(vv, ncol=nlout)
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
				vv <- matrix(vv, ncol=nlout)
				outraster <- writeValues(outraster, vv, tr$row[i])
				pbStep(pb, i)
			}
		}
		pbClose(pb)
		outraster <- writeStop(outraster)
	} 
	return(outraster)
}

