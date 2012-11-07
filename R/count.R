# Author: Robert J. Hijmans
# Date : March 2009
# Version 0.9
# Licence GPL v3


.count <- function(x, value, digits=0, progress='', ...) {

	value <- value[1]
	
	if (nlayers(x) > 1) {
	
		if (canProcessInMemory(x, 2)) {
			if (is.na(value)) {
				v <-  colSums(is.na(getValues(x)))
			} else {
				v <- round(getValues(x), digits=digits) == value
				v <- colSums(v, na.rm=TRUE)
			}
		} else {
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, label='count', progress=progress)
			v <- 0
			for (i in 1:tr$n) {
				vv <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				if (is.na(value)) {
					v <- v + colSums(is.na(vv))
				} else {
					vv <- round(v, digits=digits) == value
					v <- v + colSums(vv, na.rm=TRUE)
				}
				pbStep(pb, i)
			}
			pbClose(pb)
		}
		return(v)	
	
	} else {
	
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
			pb <- pbCreate(tr$n, label='count', progress=progress)
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
	}
}


