# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("crosstab")) {
	setGeneric("crosstab", function(x, y, ...)
		standardGeneric("crosstab"))
}


setMethod('crosstab', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, digits=0, progress, ...) {
		compare(c(x, y))
		if (missing(progress)) { progress <- .progress() }

		if (dataContent(x) == 'all' & dataContent(y) == 'all') {
			return( table(first=round(values(x), digits=digits), second=round(values(y), digits=digits)) )	
		}
		
		res=NULL
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n, type=progress)	
		for (i in 1:tr$n) {
			d <- table( round(getValuesBlock(x, row=tr$row[i], nrows=tr$size), digits=digits), round(getValuesBlock(y, row=tr$row[i], nrows=tr$size), digits=digits))
			if (length(dim(d))==1) {
				first = as.numeric(names(d))
				second = first
			} else {
				first = as.numeric(rep(rownames(d), each=ncol(d)))
				second = as.numeric(rep(colnames(d), times=nrow(d)))
			}
			count = as.vector(matrix(d))
			res = rbind(res, cbind(first, second, count))
			pbStep(pb, i)
		}
		pbClose(pb)
		res = xtabs(count~first+second, data=res)
		return(res)
	}
)

