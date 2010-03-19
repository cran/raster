# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

.startGDALwriting <- function(raster, filename, options, doPB, ...) {
	mvFlag <- NA
	attr(raster@file, "transient") <- .getGDALtransient(raster, filename=filename, mvFlag=mvFlag, options=options, ...)
	if (doPB)  { attr(raster@file, "pb") <- pbCreate(nrow(raster), type=.progress(...) ) }
	raster@file@driver <- 'gdal'
	raster@data@source <- 'disk'		
	raster@file@name <- filename
	return(raster)
}

.stopGDALwriting <- function(raster, doPB) {
	saveDataset(raster@file@transient, raster@file@name)
	GDAL.close(raster@file@transient) 
	if (doPB) {
		try (pbClose( attr(raster@file, "pb") ), silent=TRUE)
		attr(raster@file, "pb") <- ''
	}
	rasterout <- raster(raster@file@name)
	if (!raster@data@haveminmax) {
		rasterout@data@min <- raster@data@min
		rasterout@data@max <- raster@data@max
	}
	rasterout@data@haveminmax <- TRUE
#	.writeStx(rasterout) 
	return(rasterout)
}

.writeGDALrow <- function(raster, filename,  options=NULL, doPB=FALSE, ... ) {
	if (!require(rgdal)) { stop() }

	rownr <- rowFromCell(raster, dataIndices(raster)[1])
	if ( rownr == 1) {
		raster <- .startGDALwriting(raster, filename=filename, options=options, doPB=doPB, ...)
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
		x <- putRasterData(raster@file@transient, values(raster, rownr), band=1, c((rownr-1), 0)) 	
	} else {
		for (i in 1:nl) {
			x <- putRasterData(raster@file@transient, values(raster, rownr)[,i], band=i, c((rownr-1), 0)) 
		}
	}

	if (doPB) {	pbStep( attr(raster@file, "pb"), rownr ) 	}
	
	if ( rownr == nrow(raster)) {
		raster <- .stopGDALwriting(raster, doPB=doPB)
	}
	return(raster)
}
