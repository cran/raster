# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

.startGDALwriting <- function(raster, filename, options, ...) {
	
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

	if (packageDescription('rgdal')$Version > '0.6-28') {
	
		nl <- nlayers(raster)
		if (nl == 1) {
			if (raster@data@haveminmax) {
				if (inMemory(raster)) {
					statistics <- c(raster@data@min, raster@data@max, mean(raster@data@values, na.rm=TRUE), sd(raster@data@values, na.rm=TRUE))
				} else {
					statistics <- c(raster@data@min, raster@data@max, 0, 0)
				}
				b <- new("GDALRasterBand", raster@file@transient, 1)
				try ( .Call("RGDAL_SetStatistics", b, as.double(statistics), PACKAGE = "rgdal"), silent=TRUE )
			}
		} else {
			if (raster@data@haveminmax) {
				if (inMemory(raster)) {
					statistics <- cbind(raster@data@min, raster@data@max, apply(raster@data@values, 2, mean, na.rm=TRUE), apply(raster@data@values, 2, sd, na.rm=TRUE))
				} else {
					statistics <- cbind(raster@data@min, raster@data@max, 0, 0)
				}
				for (i in 1:nl) {
					b <- new("GDALRasterBand", raster@file@transient, i)
					try ( .Call("RGDAL_SetStatistics", b, as.double(statistics[i,]), PACKAGE = "rgdal"), silent=TRUE )
				}		
			}
		}
		
	}
		
	if (raster@file@options[1] == "") {
		options <- NULL
	} else {
		options <- raster@file@options	
	}
	saveDataset(raster@file@transient, raster@file@name, options=options)
	GDAL.close(raster@file@transient) 
	
	if (nl > 1) {
		out <- brick(raster@file@name)
	} else {
		out <- raster(raster@file@name)
	}
	
	if (! out@data@haveminmax) {
		out@data@min <- raster@data@min
		out@data@max <- raster@data@max
		out@data@haveminmax <- TRUE
	}

#	.writeStx(rasterout) 
	return(out)
}


