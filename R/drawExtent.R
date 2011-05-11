# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



drawExtent <- function(show=TRUE, col="red") {
	loc <- locator(n=2, type="p")
	bb <- extent(min(loc$x), max(loc$x), min(loc$y), max(loc$y))
	if (show) {
		p <- rbind(c(bb@xmin, bb@ymin), c(bb@xmin, bb@ymax), c(bb@xmax, bb@ymax), c(bb@xmax, bb@ymin), c(bb@xmin, bb@ymin) )
		lines(p, col=col)
	}
	return(bb)
}
