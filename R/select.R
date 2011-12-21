# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("select")) {
	setGeneric("select", function(x, ...)
		standardGeneric("select"))
}

	
setMethod('select', signature(x='SpatialPolygons'), 
	function(x, use='rec', draw=TRUE, col='cyan', size=2, ...) {
		require(rgeos)
		use <- substr(tolower(use),1, 3)
		stopifnot(use %in% c('rec', 'pol'))
		if (use == 'rec') {
			e <- as(drawExtent(), 'SpatialPolygons')
		} else {
			e <- drawPoly()
		}
		e@proj4string <- x@proj4string
		int <- gIntersects(x, e, byid=TRUE)
		int <- apply(int, 2, any)
		if (any(int)) {
			x <- x[int, ]
			if (draw) {
				sp::plot(x, add=TRUE, border=col, lwd=size)
			}
		} else {
			x <- NULL
		}
		invisible(x)
	}
)


	
setMethod('select', signature(x='SpatialLines'), 
	function(x, use='rec', draw=TRUE, col='cyan', size=2, ...) {
		require(rgeos)
		use <- substr(tolower(use),1, 3)
		stopifnot(use %in% c('rec', 'pol'))
		if (use == 'rec') {
			e <- as(drawExtent(), 'SpatialPolygons')
		} else {
			e <- drawPoly()
		}
		e@proj4string <- x@proj4string
		int <- gIntersects(x, e, byid=TRUE)
		int <- apply(int, 2, any)
		if (any(int)) {
			x <- x[int, ]
			if (draw) {
				sp::plot(x, add=TRUE, col=col, lwd=size)
			}
		} else {
			x <- NULL
		}			
		invisible(x)
	}
)


setMethod('select', signature(x='SpatialPoints'), 
	function(x, use='rec', draw=TRUE, col='cyan', size=2, ...) {
		require(rgeos)
		use <- substr(tolower(use),1, 3)
		stopifnot(use %in% c('rec', 'pol'))
		if (use == 'rec') {
			e <- as(drawExtent(), 'SpatialPolygons')
		} else {
			e <- drawPoly()
		}
		e@proj4string <- x@proj4string
		i <- which(!is.na(over(x, e)))
		if (length(i) > 0) {
			x <- x[i,]
			if (draw) {
				points(x, col=col, lwd=size)
			}
		} else {
			x <- NULL
		}
		invisible(x)
	}
)

