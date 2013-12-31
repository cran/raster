# Author: Robert J. Hijmans
# Date :  June 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("spplot")) {
	setGeneric("spplot", function(obj, ...)
		standardGeneric("spplot"))
}	


setMethod("spplot", signature(obj='Raster'), 
	function(obj, ..., maxpixels=50000, as.table=TRUE, zlim)  {
		obj <- sampleRegular(obj, maxpixels, asRaster=TRUE, useGDAL=TRUE)
		if (!missing(zlim)) {
			if (length(zlim) != 2) {
				warning('zlim should be a vector of two elements')
			} 
			if (length(zlim) >= 2) {
				obj[obj < zlim[1] | obj > zlim[2]] <- NA
			}
		}
		obj <- as(obj, 'SpatialGridDataFrame')
		#obj@data <- obj@data[, ncol(obj@data):1]
		spplot(obj, ..., as.table=as.table)
	}
)

# spplot for SpatialPoints object that has no data.frame
setMethod('spplot', signature(obj='SpatialPoints'), 
function(obj, ...) {
	obj <- SpatialPointsDataFrame(obj, data.frame(ID=1:length(obj)))
	spplot(obj, ...)
})

