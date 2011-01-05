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
	return(c(x, y))
}
)
