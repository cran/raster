# Author: Robert J. Hijmans
# Date :  June 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("spplot")) {
	setGeneric("spplot", function(obj, ...)
		standardGeneric("spplot"))
}	


setMethod("spplot", signature(obj='Raster'), 
	function(obj, maxpixels=50000, ...)  {
		obj <- sampleRegular(obj, maxpixels, asRaster=T)
		spplot(as(obj, 'SpatialGridDataFrame'), ...)
	}
)

