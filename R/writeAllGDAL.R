# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



.writeGDALall <- function(x, filename, options=NULL, ...) {
	
	x <- .startGDALwriting(x, filename, options, ...)
	x <- writeValues(x, values(x), start=1)
	.stopGDALwriting(x)

}
	


...writeGDALall <- function(raster, filename, options=NULL, ...) {

	if (! .requireRgdal() ) { stop('rgdal not available') }

	nl <- nlayers(raster)
	if (nl == 1) {
		out <- raster(raster)
		out <- .startGDALwriting(out, filename, options, ...) 
		raster <- as.matrix(raster)
		if (substr(out@file@datanotation, 5, 5) == 'U') {
			raster[raster < 0] <- NA
			if (out@file@datanotation <- 'INT4U') {
				out@file@nodatavalue <- 2147483647
				raster[raster > 2147483647] <- out@file@nodatavalue
			}
		} 
		raster[is.na(raster)] <- out@file@nodatavalue
		
		z <- putRasterData(out@file@transient, t(raster), band=1, c(0, 0)) 
	} else {
		out <- brick(raster, values=FALSE)
		out <- .startGDALwriting(out, filename, options, ...) 
		raster <- getValues(raster)
		if (substr(out@file@datanotation, 5, 5) == 'U') {
			raster[raster < 0] <- NA
			raster[raster < 0] <- NA
			if (out@file@datanotation <- 'INT4U') {
				out@file@nodatavalue <- 2147483647
				raster[raster > 2147483647] <- out@file@nodatavalue
			}
		} 
		raster[is.na(raster)] = out@file@nodatavalue

	    for (i in 1:nl) {
			v <- matrix(raster[,i], nrow=out@ncols, ncol=out@nrows)
			x <- putRasterData(out@file@transient, v, band=i, c(0, 0))
		}
	}	
	
	if (raster@data@haveminmax) {
		out@data@min <- raster@data@min
		out@data@max <- raster@data@max
		out@data@haveminmax <- TRUE
	}
	
	return( .stopGDALwriting(out) )
}

