# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("crosstab")) {
	setGeneric("crosstab", function(x, y, ...)
		standardGeneric("crosstab"))
}


setMethod('crosstab', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, digits=0, long=FALSE, progress, ...) {
	
		compare(c(x, y))
		if (missing(progress)) { progress <- .progress() }

		if (canProcessInMemory(x, 3) | ( inMemory(x) & inMemory(y) )) {
			res <- table(first=round(getValues(x), digits=digits), second=round(getValues(y), digits=digits)) 
		} else {
			res=NULL
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, type=progress)	
			for (i in 1:tr$n) {
			
				d <- table( round(getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]), digits=digits), round(getValuesBlock(y, row=tr$row[i], nrows=tr$nrows[i]), digits=digits))
				if (length(dim(d))==1) {
					first = as.numeric(names(d))
					second = first
					d <- matrix(d)
				} else {
					first = as.numeric(rep(rownames(d), each=ncol(d)))
					second = as.numeric(rep(colnames(d), times=nrow(d)))
				}
				count = as.vector(t(d))
				res = rbind(res, cbind(first, second, count))
				pbStep(pb, i)
			}
			pbClose(pb)
			res = xtabs(count~first+second, data=res)
		}
		
		if (long) {
			aa = as.numeric(rownames(res))
			bb = as.numeric(colnames(res))
			cc = rep(aa, length(bb))
			dd = rep(bb, each=length(aa))
			res = cbind(cc, dd, as.vector(res))
			colnames(res) <- c('first', 'second', 'value')
		}
		
		return(res)
	}
)

