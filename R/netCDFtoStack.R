# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009 / revised June 2010
# Version 1.0
# Licence GPL v3


.stackCDF <- function(filename, varname='', bands='') {

	if (!require(ncdf)) { stop('You need to install the ncdf package first') }

	nc <- open.ncdf(filename)
	on.exit( close.ncdf(nc) )

	zvar <- .varName(nc, varname)

	dims <- nc$var[[zvar]]$ndims	
	
	if (dims== 1) { 
		stop('zvar only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		stop(zvar, 'has ', length(dims), ' dimensions, I do not know how to process these data')
	} else if (dims == 2) {
		return( stack ( raster(filename, varname=zvar )  )  )
	} else {
		if (is.null(bands)) { bands <- ''}
		if (bands[1] == '') {
			bands = 1 : nc$var[[zvar]]$dim[[3]]$len
		}
		r <- raster(filename, varname=zvar, band=bands[1])
		st <- stack( r )
		st@title <- r@layernames

		if (length(bands) > 1) {
			st@zname <- nc$var[[zvar]]$dim[[3]]$units[bands]
			st@zvalue <- nc$var[[zvar]]$dim[[3]]$vals[bands]
			if ( nc$var[[zvar]]$dim[[3]]$name == 'time' ) {	
				st <- .doTime(st, nc, zvar) 
			}
			st@layers = lapply(as.list(bands), function(x){r@data@band <- x; r@layernames <- st@zvalue[x]; return(r)} )
			st@layernames <- st@zvalue
		} 
		return( st )
	}
}
	
 #s = .stackCDF(f, varname='uwnd')
