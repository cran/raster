# Author: Robert J. Hijmans
# Date :  October 2008
# revised: October 2011
# Version 1.0
# Licence GPL v3


setMethod("mean", signature(x='Raster'),
	function(x, ..., trim=NA, na.rm=FALSE){
		
		if (!is.na(trim)) {	warning("argument 'trim' is ignored") }
		
		if (as.integer(R.Version()$minor) < 15) {
			old <- TRUE
		} else {
			old <- FALSE
		}


		dots <- list(...)
		if (length(dots) > 0) {
			x <- stack(.makeRasterList(x, ...))
			add <- unlist(.addArgs(...))
		} else {
			add <- NULL
		}
		out <- raster(x)
		d <- dim(x)
		nc <- ncell(out)
		if (is.null(add)) {
			if (canProcessInMemory(x)) {
				x <- getValues(x)
				if (old) {
					x <- setValues(out, rowMeans(x, na.rm=na.rm))
				} else {
					x <- setValues(out, .rowMeans(x, nc, d[3], na.rm=na.rm))
				}
				return(x)
			}

			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='mean')
			out <- writeStart(out, filename="")
			if (old) {
				for (i in 1:tr$n) {
					v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
					v <- rowMeans(v, na.rm=na.rm)
					out <- writeValues(out, v, tr$row[i])
					pbStep(pb, i)
				}
			} else {
				for (i in 1:tr$n) {
					v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
					v <- .rowMeans(v, tr$nrows[i]*d[2], d[3], na.rm=na.rm)
					out <- writeValues(out, v, tr$row[i])
					pbStep(pb, i)
				}
			}
			pbClose(pb)
			writeStop(out)
		} else {
			d3 <- d[3] + length(add)
			if (canProcessInMemory(x)) {
				if (length(add) == 1) {
					x <- cbind(getValues(x), add)
				} else {
					x <- getValues(x)
					x <- t(apply(x, 1, function(i) c(i, add)))
				}
				if (old) {
					x <- setValues(out, rowMeans(x, na.rm=na.rm))				
				} else {
					x <- setValues(out, .rowMeans(x, nc, d3, na.rm=na.rm))
				}
				return(x)
			}

			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='mean')
			out <- writeStart(out, filename="")
			if (old) {
				for (i in 1:tr$n) {
					v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
					v <- t(apply(v, 1, function(i) c(i, add)))
					v <- rowMeans(v, na.rm=na.rm)
					out <- writeValues(out, v, tr$row[i])
					pbStep(pb, i)
				}			
			} else {
				for (i in 1:tr$n) {
					v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
					v <- t(apply(v, 1, function(i) c(i, add)))
					v <- .rowMeans(v, tr$nrows[i]*d[2], d3, na.rm=na.rm)
					out <- writeValues(out, v, tr$row[i])
					pbStep(pb, i)
				}
			}
			pbClose(pb)
			writeStop(out)
		}
	}
)




.sum <- function(x, add=NULL, na.rm=FALSE){

	if (as.integer(R.Version()$minor) < 15) {
		old <- TRUE
	} else {
		old <- FALSE
	}


	out <- raster(x)
	d <- dim(x)
	nc <- ncell(out)

	if (is.null(add)) {	
		if (canProcessInMemory(x)) {
			if (old) {
				return(  setValues(out, rowSums(getValues(x), na.rm=na.rm)) )
			} else {
				return(  setValues(out, .rowSums(getValues(x), nc, d[3], na.rm=na.rm)) )
			}
		}
		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='mean')
		out <- writeStart(out, filename="")
		
		if (old) {
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				v <- rowSums(v, na.rm=na.rm)
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			}		
		} else {
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				v <- .rowSums(v, tr$nrows[i]*d[2], d[3], na.rm=na.rm)
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			}
		}
		pbClose(pb)
		return ( writeStop(out) )
		
	
	
	} else {
		add <- sum(add, na.rm=na.rm)
		d3 <- d[3] + 1
		
		if (canProcessInMemory(x)) {
			if (old) {
				return( setValues(out, rowSums(cbind(getValues(x), add), na.rm=na.rm)) )
			} else {
				return( setValues(out, .rowSums(cbind(getValues(x), add), nc, d3, na.rm=na.rm)) )
			}
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='mean')
		out <- writeStart(out, filename="")
		if (old) {
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				v <- rowSums(cbind(v, add), na.rm=na.rm)
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			}
		} else {
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				v <- .rowSums(cbind(v, add), tr$nrows[i]*d[2], d3, na.rm=na.rm)
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			}
		}
		pbClose(pb)
		writeStop(out)
	}
}




.min <- function(x, add=NULL, na.rm=FALSE) {

	out <- raster(x)
	if (is.null(add)) {

		if (canProcessInMemory(x)) {
			return(  setValues(out, .rowMin(getValues(x), na.rm=na.rm)) )
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='mean')
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- .rowMin(v, na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		
		return ( writeStop(out) )
		

	} else {	
		add <- min(add, na.rm=na.rm)

		if (canProcessInMemory(x)) {
			x <- setValues(out, .rowMin(cbind(getValues(x), add), na.rm=na.rm))
			return(x)
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='mean')
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- .rowMin(cbind(v, add), na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		return ( writeStop(out) )
		
	}
		
}




.max <- function(x, add=NULL, na.rm=FALSE){

	out <- raster(x)
	
	if (is.null(add)) {
		if (canProcessInMemory(x)) {
			return(  setValues(out, .rowMax(getValues(x), na.rm=na.rm)) )
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='mean')
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- .rowMax( v, na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		return( writeStop(out) )
		
	} else {
		add <- max(add, na.rm=na.rm)
	
		if (canProcessInMemory(x)) {
			x <- setValues(out, .rowMax(cbind(getValues(x), add), na.rm=na.rm))
			return(x)
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='mean')
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- .rowMax( cbind(v, add), na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		return( writeStop(out) )
	}
}


