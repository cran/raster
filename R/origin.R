# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("origin")) {
	setGeneric("origin", function(x)
		standardGeneric("origin"))
}


setMethod('origin', signature(x='BasicRaster'), 
function(x) {
	e <- x@extent
	r <- res(x)
	x <- e@xmin - r[1]*(round(e@xmin / r[1]))
	y <- e@ymax - r[2]*(round(e@ymax / r[2]))
	
	if (isTRUE(all.equal((r[1] + x), abs(x)))) {
		x <- abs(x)
	}
	if (isTRUE(all.equal((r[2] + y), abs(y)))) {
		y <- abs(y)
	}
	return(c(x, y))
}
)
