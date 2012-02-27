# Author: Robert J. Hijmans
# Date :  October 2008
# revised: October 2011
# Version 1.0
# Licence GPL v3


setMethod("mean", signature(x='Raster'),
	function(x, ..., trim=NA, na.rm=FALSE){
		if (!is.na(trim)) {	warning("argument 'trim' is ignored") }

		dots <- list(...)
		if (length(dots) > 0) {
			x <- stack(.makeRasterList(x, ...))
			add <- .addArgs(...)
		} else {
			add <- NULL
		}
		out <- raster(x)
		
		if (canProcessInMemory(x)) {
			x <- cbind(getValues(x), add)
			x <- setValues(out, rowMeans(x, na.rm=na.rm))
			return(x)
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n)
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- rowMeans(cbind(v, add), na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		writeStop(out)
	}
)




.sum <- function(x, add=NULL, na.rm=FALSE){

		out <- raster(x)

		if (canProcessInMemory(x)) {
			x <- setValues(out, rowSums(cbind(getValues(x), add), na.rm=na.rm))
			return(x)
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n)
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- rowSums(cbind(v, add), na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		writeStop(out)
}




.min <- function(x, add=NULL, na.rm=FALSE) {

		out <- raster(x)
		
		if (canProcessInMemory(x)) {
			x <- setValues(out, .rowMin(cbind(getValues(x), add), na.rm=na.rm))
			return(x)
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n)
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- .rowMin(cbind(v, add), na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		writeStop(out)
}




.max <- function(x, add=NULL, na.rm=FALSE){

		out <- raster(x)
		
		if (canProcessInMemory(x)) {
			x <- setValues(out, .rowMax(cbind(getValues(x), add), na.rm=na.rm))
			return(x)
		}

		tr <- blockSize(x)
		pb <- pbCreate(tr$n)
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
			v <- .rowMax( cbind(v, add), na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		writeStop(out)
}


