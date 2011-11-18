# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("count")) {
	setGeneric("count", function(x, value, ...)
		standardGeneric("count"))
}	

setMethod('count', signature(x='RasterLayer', value='ANY'), 
function(x, value, digits=0, progress, ...) {
	if (missing(value)) { stop("'value' is missing") }
	value <- value[1]
	
	if (canProcessInMemory(x, 2)) {
		if (is.na(value)) {
			x <- sum(is.na(getValues(x)))
		} else {
			v <- na.omit(round(getValues(x), digits=digits))
			x <- sum(v == value)
		}
		return(x)
	} else {
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n)
		r <- 0
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if (is.na(value)) {
				r <- r + sum(is.na(v))
			} else {
				v <- na.omit(round(v, digits=digits))
				r <- r + sum(v == value)
			}
			pbStep(pb, i)
		}
		pbClose(pb)
		return(r)
	}
} )


setMethod('count', signature(x='RasterStackBrick', value='ANY'), 
	function(x, value, digits=0, ...) {
	if (missing(value)) { stop("'value' is missing") }
	value <- value[1]

	if (canProcessInMemory(x, 2)) {
		if (is.na(value)) {
			x <-  colSums(is.na(getValues(x)))
		} else {
			v <- round(getValues(x), digits=digits) == value
			x <-  colSums(v, na.rm=TRUE)
		}
	} else {
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n)
		x <- 0
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			if (is.na(value)) {
				x <- x + colSums(is.na(v))
			} else {
				v <- round(v, digits=digits) == value
				x <- x + colSums(v, na.rm=TRUE)
			}
			pbStep(pb, i)
		}
		pbClose(pb)
	}
	return(x)
} )




