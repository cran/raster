# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2008
# Version 0.9
# Licence GPL v3


xmin <- function(object) {
	return(extent(object)@xmin)
}

xmax <- function(object) {
	return(extent(object)@xmax)
}

ymin <- function(object) {
	return( extent(object)@ymin)
}

ymax <- function(object) {
	return (extent(object)@ymax)
}

xres <- function(object) {
	return ( (xmax(object) - xmin(object)) / ncol(object))  
}

yres <- function(object) {
	return ( (ymax(object) - ymin(object)) / nrow(object))  
}

res <- function(object) {
	return(c(xres(object), yres(object)))
}


origin <- function(object) {
	x <- xmin(object) - xres(object)*(round(xmin(object) / xres(object)))
	y <- ymax(object) - yres(object)*(round(ymax(object) / yres(object)))
	return(c(x, y))
}

