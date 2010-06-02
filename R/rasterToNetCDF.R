# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: September 2009
# Version 0.9
# Licence GPL v3


.writeRasterCDF <- function(x, filename, ...){ 
	
	if (!require(RNetCDF)) { stop() }	

 	filename <- trim(filename)
	
#	cat(filename, '\n')
	
	overwrite <- .overwrite(...)
		
	xvar <- (xmin(x) + 1:ncol(x) * xres(x)) - 0.5 * xres(x)
	yvar <- (ymin(x) + 1:nrow(x) * yres(x)) - 0.5 * yres(x)

	zvar <- getValues(x)
	if (class(zvar) == 'numeric') {
		dtype <- 'NC_DOUBLE'
	} else {
		dtype <- 'NC_INT' 
	}
		
		zvar <- matrix(zvar, ncol=ncol(x), nrow=nrow(x), byrow=TRUE)
		zvar <- t(zvar[nrow(zvar):1, ])
		
		nc <- create.nc(filename, clobber=overwrite)
		dim.def.nc(nc, "x", ncol(x))
		dim.def.nc(nc, "y", nrow(x))
		
		var.def.nc(nc, "x", 'NC_DOUBLE', 0)
		var.def.nc(nc, "y", 'NC_DOUBLE', 1)
		var.def.nc(nc, "z", 'NC_DOUBLE', c(0,1))
		
		att.put.nc(nc, "z", "missing_value", "NC_DOUBLE", NAvalue(x))
 	    att.put.nc(nc, "z", "long_name", "NC_CHAR", layerNames(x))
		
		att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Data from the R raster package")
		att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR", paste("Created on", date()))

		var.put.nc(nc, "x", xvar)
		var.put.nc(nc, "y", yvar)
		var.put.nc(nc, "z", zvar)


		close.nc(nc)
}



.writeStackCDF <- function(x, y){ 
		if (!require(RNetCDF)) { stop() }	
		stop('not implemented yet')
}



