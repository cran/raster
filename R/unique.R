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
		pb <- pbCreate(tr$n, progress=progress)	

		for (i in 1:tr$n) {
			u1 <- unique( c(u1, getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i])) )
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


setMethod('unique', signature(x='RasterStackBrick', incomparables='missing'), 
function(x, progress='') {
	
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			if (canProcessInMemory(x, 2)) {
				x <- readAll(x)
			}
		} else {
			stop('This object has no values')	
		}
	} 

	if ( inMemory(x) ) {
	
		x <- unique(getValues(x))
		if (is.list(x)) {
			for (i in 1:length(x)) {
				x[[i]] <- sort(x[[i]])
			}
		} else {
			xx <- vector(length=ncol(x), mode='list')
			for (i in 1:ncol(x)) {
				xx[[i]] <- sort(x[,i])
			}
		}
		return(x)
		
	} else {
		nl <- nlayers(x)
		un <- list(length=nl, mode='list')
		
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n, progress=progress)	

		un <- NULL
		for (i in 1:tr$n) {
			v <- unique( getValues(x, row=tr$row[i], nrows=tr$nrows[i]) )
			un <- unique(rbind(v, un))
			pbStep(pb, i)			
		}
		pbClose(pb)
		return(un)	
	}
}
)

