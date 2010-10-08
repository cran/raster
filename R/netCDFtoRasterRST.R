# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 0.9
# Licence GPL v3

.rasterObjectFromCDFrst <- function(filename, band=NA, type='RasterLayer', ...) {

	if (!require(ncdf)) { stop('You need to install the ncdf package first') }

	nc <- open.ncdf(filename)
	on.exit( close.ncdf(nc) )

	datatype <- .getRasterDTypeFromCDF(nc$var[[1]]$prec )
	
	ncols <- att.get.ncdf(nc, 0, 'ncols')
	nrows <- att.get.ncdf(nc, 0, 'nrows')
	xmn <- att.get.ncdf(nc, 0, 'xmin')
	xmx <- att.get.ncdf(nc, 0, 'xmax')
	ymn <- att.get.ncdf(nc, 0, 'ymin')
	ymx <- att.get.ncdf(nc, 0, 'ymax')
	crs <- att.get.ncdf(nc, 0, 'crs')
	
	if (type == 'RasterLayer') {
		r <- raster(xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, ncols=ncols, nrows=nrows, crs=crs)
	} else {
		r <- brick(xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymax, ncols=ncols, nrows=nrows, crs=crs)
	}
	r@file@name <- filename

	r@file@driver <- "netcdf"	
	r@data@fromdisk <- TRUE

	dims <- nc$var[[1]]$ndims
	
	r@file@nbands <- nc$var[[1]]$dim[[3]]$len
	
	if (is.na(band)) {
		r@file@band <- 1
	} else {
		r@file@band <- min(max(1, band), r@file@nbands)
	}

	r@data@min <- att.get.ncdf(nc, "value", 'min')
	r@data@max <- att.get.ncdf(nc, "value", 'max')

	return(r)
}

