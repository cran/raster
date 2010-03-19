# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.9
# Licence GPL v3



'res<-' <- function(object, value) {
	if (length(value) == 1) {
		xr=value
		yr=value
	} else {
		xr=value[1]
		yr=value[2]
	}
	
	bb <- extent(object)
	nc <- round( (bb@xmax - bb@xmin) / xr )
	nr <- round( (bb@ymax - bb@ymin) / yr )
	if (nr != object@nrows | nc != object@ncols) {
		if (extends(class(object), "Raster")) {
			object <- clearValues(object)
		}
	}
	bb@xmax <- bb@xmin + nc * xr
	bb@ymin <- bb@ymax - nr * yr
	extent(object) <- bb
	rowcol(object) <- c(nr, nc)
	return(object)
}

