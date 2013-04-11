# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 1.0
# Licence GPL v3


setReplaceMethod("[", c("RasterLayer", "RasterLayer", "missing"),
	function(x, i, j, value) {

		if (! hasValues(i) ) {
			i <- cellsFromExtent(x, i)
			
		} else if (compareRaster(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
			i <- as.logical( getValues(i) )
		
		} else {
			i <- cellsFromExtent(x, i)
		}		
	
		.replace(x, i, value=value, recycle=1) 
	}
)



setReplaceMethod("[", c("RasterLayer","missing","missing"),
	function(x, i, j, value) {
	
		if (length(value) == ncell(x)) {
			x <- try( setValues(x, value))
		} else if (length(value) == 1) {
			x <- try( setValues(x, rep(value, times=ncell(x))) )
		} else {
			v <- try( vector(length=ncell(x)) )
			if (class(x) != 'try-error') {
				v[] <- value
				x <- try( setValues(x, v) )
			}
		}
		if (class(x) == 'try-error') {
			stop('cannot replace values on this raster (it is too large')
		}
		return(x)
	
	}
)


.replace <- function(x, i, value, recycle=1) {

	if (inherits(x, 'RasterStack')) {
		x <- brick(x, values=TRUE)
	}
	if ( is.logical(i) ) {
		i <- which(i)
	} else {
	#	if (! is.numeric(i)) { 
	#		i <- as.integer(i) 
	#	}
		i <- na.omit(i)
	}

	nl <- nlayers(x)
  # recycling
	if (nl > 1) {
		rec2 <- ceiling(nl / recycle)
		if (rec2 > 1) {
			add <- ncell(x)*recycle * (0:(rec2-1))
			i <- as.vector(t((matrix(rep(i, rec2), nrow=rec2, byrow=TRUE)) + add))
		}
	}
	j <- i > 0 & i <= (ncell(x)*nl)
	
	if (!all(j)) {
		i <- i[j]
		value <- value[j]
	}
	
	if (! inMemory(x) ) {
		if (canProcessInMemory(x)) {
			if ( fromDisk(x) ) {
				x <- readAll(x)
			} else {
				x <- setValues(x, rep(NA, times=ncell(x)))
			}
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='replace')
			hv <- hasValues(x)
			if (nl==1) {
				r <- raster(x)
				r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
				for (k in 1:tr$n) {
					cells <- cellFromRowCol(x, tr$row[k], 1):cellFromRowCol(x, tr$row[k]+tr$nrows[k]-1, ncol(x))
					if (hv) {
						v <- getValues(x, row=tr$row[k], nrows=tr$nrows[k])
					} else {
						v <- rep(NA, length(cells))
					}
					j <- which(i %in% cells)
					if (length(j) > 0) {
						localcells <- i[j] - (cells[1]-1)
						v[localcells] <- value[j]
					}
					r <- writeValues(r, v, tr$row[k])
					pbStep(pb, k) 	
				}
				
			} else {
				r <- brick(x)
				r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
				add <- (0:(nl-1)) * ncell(x)
				for (k in 1:tr$n) {
					cells <- cellFromRowCol(x, tr$row[k], 1):cellFromRowCol(x, tr$row[k]+tr$nrows[k]-1, ncol(x))
					if (hv) {
						v <- getValues(x, row=tr$row[k], nrows=tr$nrows[k])
					} else {
						v <- matrix(NA, nrow=length(cells), ncol=nl)
					}
					cells <- cells + rep(add, each=length(cells))
					j <- which(i %in% cells)
					if (length(j) > 0) {
						localcells <- i[j] - (cells[1]-1)
						v[localcells] <- value[j]
					}
					r <- writeValues(r, v, tr$row[k])
					pbStep(pb, k)
				}
			}	
			r <- writeStop(r)
			pbClose(pb)
			return(r)
		}
	}
		
	x@data@values[i] <- value
	x <- setMinMax(x)
	x <- .clearFile(x)
	return(x)
}


