# R function for the raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3


	
if (!isGeneric("zoom")) {
	setGeneric("zoom", function(x, ...)
		standardGeneric("zoom"))
}	


setMethod('zoom', signature(x='Raster'), 
function(x, extent=drawExtent(), maxpixels=100000, layer=1, new=TRUE, ...) {
	if (missing(x)) {
		stop('You must provide a Raster* object as first argument to this function')
	}
	extent <- extent  # force to start with drawing before creating a new graphics device
	if (new) { dev.new() }
	if (class(x) != 'RasterLayer') { x <- raster(x,layer) }
	.plotraster(x, maxpixels=maxpixels, extent=extent, ...) 	
	return(invisible(extent))
}
)
