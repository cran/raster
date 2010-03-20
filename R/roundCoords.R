# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


roundExtent <- function(object, digits=0) {
	digits <- max(0, digits)
	b <- extent(object)
	b@xmin <- round(b@xmin, digits)
	b@xmax <- round(b@xmax, digits)
	b@ymin <- round(b@ymin, digits)
	b@ymax <- round(b@ymax, digits)
	if (class(object) == 'Extent') {
		return(b)
	}
	extent(object) <- b
	return(object)
}

nudgeExtent <- function(object){
	b <- extent(object)
	b@xmin <- floor(b@xmin)
	b@ymin <- floor(b@ymin)
	b@xmax <- ceiling(b@xmax)
	b@ymax <- ceiling(b@ymax)
	if (class(object) == 'Extent') {
		return(b)
	}
	extent(object) <- b
	return(object)
}
