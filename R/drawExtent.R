# R function for the raster package
# Author: Robert J. Hijmans
# Date : January 2009, December 2011
# Version 1.0
# Licence GPL v3



drawExtent <- function(show=TRUE, col="red") {
	if (show) {
		loc1 <- locator(n=1, type="p", pch='+', col=col)
	} else {
		loc1 <- locator(n=1)	
	}
	loc2 <- locator(n=1)
	loc <- rbind(unlist(loc1), unlist(loc2))
	bb <- extent(min(loc[,'x']), max(loc[,'x']), min(loc[,'y']), max(loc[,'y']))
	if (show) {
		p <- rbind(c(bb@xmin, bb@ymin), c(bb@xmin, bb@ymax), c(bb@xmax, bb@ymax), c(bb@xmax, bb@ymin), c(bb@xmin, bb@ymin) )
		lines(p, col=col)
	}
	return(bb)
}
