# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.1
# Licence GPL v3


shapefile <- function(filename, object=NULL, overwrite=FALSE, verbose=FALSE) {
	if (!(require(rgdal))) {
		stop('This function requires the rgdal package; please install it')
	}
	if (is.null(object)) {
	
		fn <- basename(filename) 
		extension(fn) <- ''
		readOGR(dirname(filename), fn, stringsAsFactors=FALSE, verbose=verbose) 
		
	} else {
		extension(filename) <- '.shp'
		if (file.exists(filename)) {
			if (!overwrite) {
				stop('file exists, use overwrite=TRUE to overwrite it')
			} 
		}
		layer <- basename(filename)
		extension(layer) <- ''
		writeOGR(object, filename, layer, driver='ESRI Shapefile', verbose=verbose, overwrite_layer=overwrite)
	}
}


