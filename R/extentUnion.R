# Authors: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

unionExtent <- function(x, ...) {
	objects <- c(x, list(...))
	if (length(objects) == 1) {
		return(extent(x))
	}
	e <- extent(objects[[1]])
	for (i in 2:length(objects)) {
		e2 <- extent(objects[[i]])
		e@xmin <- min(e@xmin, e2@xmin)
		e@xmax <- max(e@xmax, e2@xmax)
		e@ymin <- min(e@ymin, e2@ymin)
		e@ymax <- max(e@ymax, e2@ymax)
	}
	return(e)
}

intersectExtent <- function(x, ..., validate=TRUE) {
	objects <- c(x, list(...))
	if (length(objects) == 1) {
		return(extent(x))
	}
	e <- extent(objects[[1]])
	for (i in 2:length(objects)) {
		e2 <- extent(objects[[i]])
		e@xmin <- max(e@xmin, e2@xmin)
		e@xmax <- min(e@xmax, e2@xmax)
		e@ymin <- max(e@ymin, e2@ymin)
		e@ymax <- min(e@ymax, e2@ymax)
	}
	if ((e@xmax <= e@xmin) | (e@ymax <= e@ymin) ) {
		if (validate) {
			stop('Invalid extent')
		} else {
			return(NULL)
		}
	}
	return(e)
}

