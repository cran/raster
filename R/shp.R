# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.1
# Licence GPL v3


shapefile <- function(filename, object=NULL, overwrite=FALSE, verbose=FALSE, stringsAsFactors=FALSE, ...) {

	.requireRgdal() 
	
	if (is.null(object)) {
		stopifnot(file.exists(extension(filename, '.shp')))
		stopifnot(file.exists(extension(filename, '.shx')))
		stopifnot(file.exists(extension(filename, '.dbf')))
		fn <- basename(filename) 
		extension(fn) <- ''
		readOGR(dirname(filename), fn, stringsAsFactors=stringsAsFactors, verbose=verbose, ...) 
		
	} else {
		extension(filename) <- '.shp'
		if (file.exists(filename)) {
			if (!overwrite) {
				stop('file exists, use overwrite=TRUE to overwrite it')
			} 
		}
		layer <- basename(filename)
		extension(layer) <- ''
		if (!inherits(object, 'Spatial')) {
			stop('To write a shapefile you need to provide an object of class Spatial*')
		} else {
			if (inherits(object, 'SpatialGrid')| inherits(object, 'SpatialPixels')) {
				stop('These data cannot be written to a shapefile')
			}
			if (!.hasSlot(object, 'data')) {
				if (inherits(object, 'SpatialPolygons')) {
					object <- SpatialPolygonsDataFrame(object, data.frame(ID=1:length(row.names(object))))
				} else if (inherits(object, 'SpatialLines')) {
					object <- SpatialLinesDataFrame(object, data.frame(ID=1:length(row.names(object))))
				} else if (inherits(object, 'SpatialPoints')) {
					object <- SpatialPointsDataFrame(object, data.frame(ID=1:length(row.names(object))))
				} else {
					stop('These data cannot be written to a shapefile')
				}
			}
		}
		
		writeOGR(object, filename, layer, driver='ESRI Shapefile', verbose=verbose, overwrite_layer=overwrite, ...)
	}
}


