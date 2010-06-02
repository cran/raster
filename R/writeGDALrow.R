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
	
	raster@file@driver <- 'gdal'
	raster@data@source <- 'disk'		
	raster@file@name <- filename
	return(raster)
}


.stopGDALwriting <- function(raster) {

	saveDataset(raster@file@transient, raster@file@name)
	GDAL.close(raster@file@transient) 
	
	if (class(raster) == 'RasterBrick') {
		rasterout <- brick(raster@file@name)
	} else {
		rasterout <- raster(raster@file@name)
	}
	if (!raster@data@haveminmax) {
		rasterout@data@min <- raster@data@min
		rasterout@data@max <- raster@data@max
	}
	rasterout@data@haveminmax <- TRUE
#	.writeStx(rasterout) 
	return(rasterout)
}


.writeGDALrow <- function(raster, filename,  options=NULL, ... ) {

	if (! .requireRgdal() ) { stop('rgdal not available') }

	rownr <- rowFromCell(raster, dataIndices(raster)[1])
	if ( rownr == 1) {
		raster <- .startGDALwriting(raster, filename=filename, options=options, ...)
	}	
	
#	raster@data@values[is.nan(raster@data@values)] <- NA
#	raster@data@values[is.infinite(raster@data@values)] <- NA
#	if (raster@file@dtype == "INT" || raster@file@dtype =='LOG' ) { 
#		values <- as.integer(round(raster@data@values))  
#		values[is.na(values)] <- as.integer(raster@file@nodatavalue)		
#	} else { 
#		values  <- as.numeric( raster@data@values ) 
#	}
	
	if (!raster@data@haveminmax) {
		rsd <- na.omit(raster@data@values) # min and max values
		if (length(rsd) > 0) {
			raster@data@min <- min(raster@data@min, min(rsd))
			raster@data@max <- max(raster@data@max, max(rsd))
		}	
	}
	
    nl <- nlayers(raster)
	if (nl == 1) {
		v <- raster@data@values
		v[is.na(v)] <- raster@file@nodatavalue
		x <- putRasterData(raster@file@transient, v, band=1, c((rownr-1), 0)) 	
	} else {
		for (i in 1:nl) {
			v <- raster@data@values[,i]
			v[is.na(v)] <- raster@file@nodatavalue
			x <- putRasterData(raster@file@transient, v, band=i, c((rownr-1), 0)) 
		}
	}

	if ( rownr == nrow(raster)) {
		raster <- .stopGDALwriting(raster)
	}
	return(raster)
}

