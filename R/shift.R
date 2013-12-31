# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("shift")) {
	setGeneric("shift", function(object, ...)
		standardGeneric("shift"))
}	


setMethod('shift', signature(object='Raster'), 
	function(object, x=0, y=0, filename='', ...) {
		x <- as.numeric(x[1])
		y <- as.numeric(y[1])
		stopifnot(!is.na(x) | !is.na(y))
		e <- object@extent
		e@xmin <- e@xmin + x
		e@ymin <- e@ymin + y
		e@xmax <- e@xmax + x
		e@ymax <- e@ymax + y
		object@extent <- e
		if (filename != '') {
			object <- writeRaster(object, filename=filename, ...)
		}
		if (inherits(object, 'RasterStack')) {
			object@layers <- sapply(object@layers, function(x){ extent(x) <- e; x})
		}
		return(object)
	}
)



setMethod('shift', signature(object='SpatialPolygons'), 
	function(object, x=0, y=0, ...) {
		a <- as.data.frame(object, xy=TRUE, centroids=FALSE)
		a$x <- a$x + x
		a$y <- a$y + y
		if (inherits(object, 'SpatialPolygonsDataFrame')) {
			as(a, 'SpatialPolygonsDataFrame')	
		} else {
			as(a, 'SpatialPolygons')			
		}
	}
)



setMethod('shift', signature(object='SpatialLines'), 
	function(object, x=0, y=0, ...) {
		a <- as.data.frame(object, xy=TRUE)
		a$x <- a$x + x
		a$y <- a$y + y
		if (inherits(object, 'SpatialLinesDataFrame')) {
			as(a, 'SpatialLinesDataFrame')	
		} else {
			as(a, 'SpatialLines')			
		}
	}
)


