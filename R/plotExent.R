# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


.extentMatrix <- function(x) {
	xy <- matrix(NA, nrow=5, ncol=2)
	xy[1,1] <- x@xmin
	xy[1,2] <- x@ymax
	xy[2,1] <- x@xmax
	xy[2,2] <- x@ymax
	xy[3,1] <- x@xmax
	xy[3,2] <- x@ymin
	xy[4,1] <- x@xmin
	xy[4,2] <- x@ymin
	return(xy)
}


setMethod("plot", signature(x='Extent', y='ANY'), 
	function(x, y, add=FALSE, ...)  {
		xy <- .extentMatrix(x)
		xy[5,] <- xy[1,]
		if (add) {
			lines(xy, ...) 
		} else {
			plot(xy, type='l', ...)
		}
		if (!missing(y)) {
			if (class(y) == 'Extent') {
				plot(x=y, add=TRUE, ...)
			}
		}
	}
)	

