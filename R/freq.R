# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("freq")) {
	setGeneric("freq", function(x, ...)
		standardGeneric("freq"))
}


setMethod('freq', signature(x='RasterLayer'), 
	function(x, digits=0, ...) {

		if (canProcessInMemory(x, 3)) {
	
			d <- round(getValues(x), digits=digits)
			res <- table(d, useNA="ifany" )
		
		} else {
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, ...)	
			z <- vector(length=0)
			for (i in 1:tr$n) {
				d <- round(getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]), digits=digits)
				res <- table(d, useNA="ifany" )
				res <- cbind(as.numeric(unlist(as.vector(dimnames(res)))), as.vector(res))
				z <- rbind(z, res)
				pbStep(pb, i)
			}
			res <- tapply(z[,2], z[,1], sum)	
			pbClose(pb)		
		}
	
		res <- cbind(as.numeric(unlist(as.vector(dimnames(res)))), as.vector(res))
		colnames(res) <- c('value', 'count')
		return(res)
	}
)

