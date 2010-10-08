# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

.startGDALwriting <- function(raster, filename, options, NAvalue, ...) {
	
	temp <- .getGDALtransient(raster, filename=filename, options=options, ...)
	
	attr(raster@file, "transient") <- temp[[1]]
	raster@file@nodatavalue <- temp[[2]]
	
	attr(raster@file, "options") <- temp[[3]]
	
	
	raster@file@driver <- 'gdal'

	raster@data@fromdisk <- TRUE
	
	raster@file@name <- filename
	return(raster)
}


.stopGDALwriting <- function(raster) {

	if (raster@file@options[1] == "") {
		options <- NULL
	} else {
		options <- raster@file@options	
	}
	
	saveDataset(raster@file@transient, raster@file@name, options=options)
	GDAL.close(raster@file@transient) 
	
	if (inherits(raster, 'RasterBrick')) {
		rasterout <- brick(raster@file@name)
	} else {
		rasterout <- raster(raster@file@name)
	}
	
	if (! rasterout@data@haveminmax) {
		rasterout@data@min <- raster@data@min
		rasterout@data@max <- raster@data@max
		rasterout@data@haveminmax <- TRUE
	}

#	.writeStx(rasterout) 
	return(rasterout)
}


