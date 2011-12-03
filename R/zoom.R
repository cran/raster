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
function(x, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, useRaster=TRUE, ...) {
	if (missing(x)) {
		stop('You must provide a Raster* object as first argument to this function')
	}
	ext <- ext  # force to start with drawing before creating a new graphics device
	if (new) { dev.new() }
	if (nlayers(x) > 1) { 
		x <- raster(x, layer) 
	}
	if (useRaster) {
		.plotraster2(x, maxpixels=maxpixels, ext=ext, ...) 	
	} else {
		.plotraster(x, col=col, maxpixels=maxpixels, add=add, ...) 
	}
		
	return(invisible(ext))
}
)
