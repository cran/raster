# Author: Robert J. Hijmans
# Contributors: Reinhard Krug
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.overlayList <- function(x, fun, filename="", ...){ 
	
	if (length(x) < 1) { stop('no RasterLayers') }
	compare(x)

	filename <- trim(filename)
	outraster <- raster(x[[1]])
	
	inram <- TRUE
	for (i in 1:length(x)) {
		if (dataContent(x[[i]]) != 'all') {
			inram <- FALSE
		} 
	}	
	if (!canProcessInMemory(outraster,2)) {inram <- FALSE }

# what kind of function is this... 
# I must be overlooking a simpler approach here	
	a <- rep(1,10)
	tr <- try ( vals <- do.call(fun, list(a)), silent=TRUE )
	if (class(tr) == "try-error") {
		applymethod = FALSE
		vlist <- list()
		for (i in 1:length(x)) {
			vlist[[i]] <- a
		}
		tr <- try ( vals <- do.call(fun, vlist), silent=TRUE ) 
		if (class(tr) == "try-error") {
			stop('cannot use this formula')
		} 
		if (length(vals) != length(a)) {
			stop('cannot use this formula; lenghts do not match')	
		}
		
	} else {
		if (length(vals) == 1 & ncol(outraster) > 1) {
			m <- matrix(rep(a,length(x)), ncol=length(x), nrow=length(a))
			vals <- apply(m, 1, fun)
			if (length(vals) == length(a)) {
				applymethod = TRUE
			} else {
				stop('cannot use this formula')
			}
		}
	}

	vallist <- list()

	if ( inram ) {
		pb <- pbCreate(3, type=.progress(...))			
		pbStep(pb, 1)
		if (applymethod) {
			valmat <- vector()
			for (i in 1:length(x)) {
				valmat <- cbind(valmat, values(x[[i]]))
				x[[i]] <- clearValues(x[[i]])
			}	
			pbStep(pb, 2)
			vals <- apply(valmat, 1, fun)
		} else {
			for (i in 1:length(x)) {
				vallist[[i]] <- values(x[[i]])
				x[[i]] <- clearValues(x[[i]])
			}
			pbStep(pb, 2)
			vals <- do.call(fun, vallist)
		}
		
		outraster <- setValues(outraster, vals)
		if (filename != "") { 
			outraster <- writeRaster(outraster, filename=filename, ...) 
		}
		pbStep(pb, 3)
		pbClose(pb)
		
	} else {
		if (filename == "") {
			if (!canProcessInMemory(outraster, 3)) {
				filename <- rasterTmpFile()
			} 
		} 
		if (filename == "") {
			v <- matrix(ncol=nrow(outraster), nrow=ncol(outraster))
		} else {
			outraster <- writeStart(outraster, filename=filename, ...)
		}
		
		
		tr <- blockSize(outraster, n=length(x))
		pb <- pbCreate(tr$n, type=.progress(...))			

		
		if (applymethod) { valmat = matrix(nrow=tr$size*ncol(outraster) , ncol=length(x)) }
		
		for (i in 1:tr$n) {
			if (applymethod) {
				if (i == tr$n) {
					valmat = matrix(nrow=tr$nrows[i]*ncol(outraster) , ncol=length(x))
				}
				for (j in 1:length(x)) {
					valmat[,j] <- getValuesBlock(x[[j]], row=tr$row[i], nrows=tr$size)
				}	
				vv <- apply(valmat, 1, fun)
				
			} else {
			
				for (j in 1:length(x)) {
					vallist[[j]] <- getValuesBlock(x[[j]], row=tr$row[i], nrows=tr$size)
				}	
				vv <- do.call(fun, vallist)
			}
		
			if (filename == "") {
				vv <- matrix(vv, nrow=ncol(outraster))
				cols <- tr$row[i]:(tr$row[i]+dim(vv)[2]-1)	
				v[,cols] <- vv

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
	} 
	return(outraster)
}
