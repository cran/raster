# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3


setMethod("+", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		require(rgeos)
		merge(e1, e2)
	}
)

setMethod("+", signature(e1='SpatialPolygonsDataFrame', e2='data.frame'),
    function(e1, e2){ 
		merge(e1, e2)
	}
)


setMethod("*", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		require(rgeos)
		crop(e1, e2)
	}
)


.gDif <- function(x, y) {
	xln <- length(x@polygons)
	yln <- length(y@polygons)
	if (xln==0 | yln==0) {
		return(x)
	}
	rn <- row.names(x)
	for (i in xln:1) {
		z <- x[i,]
		for (j in 1:yln) {
			z <- gDifference(z, y[j,])
			if (is.null(z)) {
				break
			}
		}
		if (is.null(z)) {
			x <- x[-i,]
			rn <- rn[-i]
		} else {
			x@polygons[i] <- z@polygons
		}
	}
	if (length(rn) > 0) {
		row.names(x) <- rn
	}
	x
}


setMethod("-", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
	
		require(rgeos)

		if (! identical(e1@proj4string, e2@proj4string) ) {
			warning('non identical CRS')
			e2@proj4string <- e1@proj4string
		}
		
		
		if (!.hasSlot(e1, 'data')) {
			d <- data.frame(ID=1:length(e1@polygons))
			rownames(d) <- row.names(e1)
			e1 <- SpatialPolygonsDataFrame(e1, data=d)
			dropframe <- TRUE
		} else {
			dropframe <- FALSE
		}

		e2 <- aggregate(e2)
		
		int <- gIntersects(e1, e2, byid=TRUE)
		int1 <- apply(int, 2, any)
		int2 <- apply(int, 1, any)
				
		if (sum(int1) == 0) { # no intersections
			return(e1)
		}
		
		if (all(int1)) {
			part1 <- NULL
		} else {
			part1 <- e1[!int1,]
		}
		part2 <- .gDif(e1[int1,], e2[int2,])

		part2 <- SpatialPolygonsDataFrame(part2, e1@data[match(row.names(part2), rownames(e1@data)), ,drop=FALSE])
		if (!is.null(part1)) {
			part2 <- rbind(part1, part2)
		}
			
		if (length(part2@polygons) > 1) {	
			part2 <- aggregate(part2, v=colnames(part2@data))
		}
		if (dropframe) {
			as(part2, 'SpatialPolygons')
		} else {
			part2	
		}
	}
)



setMethod("^", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		
		stop('not yet implemented (symetric difference)')
	}
)
