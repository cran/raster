# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3


.explode <- function(x, df=FALSE) {
	npols <- length(x@polygons)
	crs <- x@proj4string
	count <- 0
	p <- NULL
	np <- vector(length=npols)
	for (i in npols) {
		parts <- x@polygons[[i]]@Polygons
		np[i] <- length(parts)
		p <- c(p, sapply(1:np[i], function(x) Polygons(parts[x], x+count)))
	}
	p <- SpatialPolygons(p)
	p@proj4string <- crs
	
	if (df) {
		if (.hasSlot(x, 'data')) {
			np <- rep(1:npols, np)
			x <- x@data[np,]
			rownames(x) <- 1:nrow(x)
			p <- SpatialPolygonsDataFrame(p, data=x)
		}
	}
	p
}

