# Author: Robert J. Hijmans 
# Date :  October 2008
# revised: October 2011
# Version 1.0
# Licence GPL v3


setMethod("modal", signature(x='Raster'),
	function(x, ..., ties='random', na.rm=FALSE){

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
			x <- setValues(out, apply(x, 1, modal, ties=ties, na.rm=na.rm))
			return(x)
		}

		tr <- blockSize(out)
		pb <- pbCreate(tr$n)
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- cbind( getValues( x, row=tr$row[i], nrows=tr$nrows[i] ), add)
			v <- apply(v, 1, modal, ties=ties, na.rm=na.rm)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		writeStop(out)
	}
)

