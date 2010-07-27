# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("unique")) {
	setGeneric("unique", function(x, incomparables=FALSE, ...)
		standardGeneric("unique"))
}	


setMethod('unique', signature(x='RasterLayer', incomparables='missing'), 
function(x, progress='') {
	
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			if (canProcessInMemory(x, 2)) {
				x <- readAll(x)
			}
		} else {
			stop('This RasterLayer has no values')	
		}
	} 

	if ( inMemory(x) ) {
		x <- unique(x@data@values)
		return(sort(x))
	} else {
		u1 <- vector()
		u2 <- vector()
		
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n, type=progress)	

		for (i in 1:tr$n) {
			u1 <- unique( c(u1, getValuesBlock(x, row=tr$row[i], nrows=tr$size)) )
			if (length(u1) > 10000 ) {
				u2 <- unique(c(u1, u2))
				u1 <- vector()
			}
			pbStep(pb, i)			
		}
		pbClose(pb)
		return(sort(unique(c(u1, u2))))	
	}
}
)

