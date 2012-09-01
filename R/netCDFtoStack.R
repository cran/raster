# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009 / revised June 2010
# Version 1.0
# Licence GPL v3


.stackCDF <- function(filename, varname='', bands='') {

	ncdf4 <- .NCDFversion4()

	if (ncdf4) {
		nc <- ncdf4::nc_open(filename)
		on.exit( ncdf4::nc_close(nc) )		
		
	} else {
		nc <- open.ncdf(filename)
		on.exit( close.ncdf(nc) )		
	} 

	zvar <- .varName(nc, varname)
	dims <- nc$var[[zvar]]$ndims	
	
	dim3 <- 3
	if (dims== 1) { 
		stop('variable only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		dim3 <- dims
		warning(zvar, ' has ', dims, ' dimensions, I am using the last one')
	} else if (dims == 2) {
		return( stack ( raster(filename, varname=zvar )  )  )
	} else {
		if (is.null(bands)) { bands <- ''}
		if (bands[1] == '') {
			bands = 1 : nc$var[[zvar]]$dim[[dim3]]$len
		}
		r <- raster(filename, varname=zvar, band=bands[1])
		st <- stack( r )
		st@title <- names(r)

		if (length(bands) > 1) {
			names(st@z) <- nc$var[[zvar]]$dim[[dim3]]$units[bands]
			st@z <- list( nc$var[[zvar]]$dim[[dim3]]$vals[bands] )
			if ( nc$var[[zvar]]$dim[[dim3]]$name == 'time' ) {	
				st <- try( .doTime(st, nc, zvar, dim3, ncdf4)  )
			}
			st@layers <- lapply(list(bands), function(x){
												r@data@band <- x;
												r@data@names <- st@z[[1]][x];
												return(r)} 
											)
		} 
		return( st )
	}
}
	
 #s = .stackCDF(f, varname='uwnd')
