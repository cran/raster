# Author: Robert J. Hijmans
# Date :  June 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("spplot")) {
	setGeneric("spplot", function(obj, ...)
		standardGeneric("spplot"))
}	


setMethod("spplot", signature(obj='Raster'), 
	function(obj, ..., maxpixels=50000, as.table=TRUE)  {
		obj <- sampleRegular(obj, maxpixels, asRaster=T)
		obj <- as(obj, 'SpatialGridDataFrame')
		#obj@data <- obj@data[, ncol(obj@data):1]
		spplot(obj, ..., as.table=as.table)
	}
)

