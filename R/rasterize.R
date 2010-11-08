# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2010
# Version 1.0
# Licence GPL v3


if (!isGeneric("rasterize")) {
	setGeneric("rasterize", function(x, y, ...)
		standardGeneric("rasterize"))
}	


setMethod('rasterize', signature(x='matrix', y='Raster'), 
function(x, y, ...){ 
	return( .pointsToRaster(x, y, ...))
})


setMethod('rasterize', signature(x='SpatialPoints', y='Raster'), 
function(x, y, ...){ 
	return( .pointsToRaster(x, y, ...))
})


setMethod('rasterize', signature(x='SpatialLines', y='Raster'), 
function(x, y, ...){ 
	.linesToRaster(x, y, ...)
})


setMethod('rasterize', signature(x='SpatialPolygons', y='Raster'), 
function(x, y, ...){ 
	.polygonsToRaster(x, y, ...)
})

setMethod('rasterize', signature(x='Extent', y='Raster'), 
 	function(x, y, ...) {
		y <- polygonFromExtent(y)
		.polygonsToRaster(x, y, ...)
	}
)
