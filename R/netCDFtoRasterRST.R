# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 0.9
# Licence GPL v3

.rasterObjectFromCDFrst <- function(filename, band=NA, type='RasterLayer', ...) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package first') }

	nc <- open.nc(filename)

	varinfo <- try(var.inq.nc(nc, 'value'))
	
	datatype <- .getRasterDTypeFromCDF(varinfo$type)
	
	ncols <- att.get.nc(nc, "NC_GLOBAL", 'ncols')
	nrows <- att.get.nc(nc, "NC_GLOBAL", 'nrows')
	xmn <- att.get.nc(nc, "NC_GLOBAL", 'xmin')
	xmx <- att.get.nc(nc, "NC_GLOBAL", 'xmax')
	ymn <- att.get.nc(nc, "NC_GLOBAL", 'ymin')
	ymx <- att.get.nc(nc, "NC_GLOBAL", 'ymax')
	crs <- att.get.nc(nc, "NC_GLOBAL", 'crs')
	
	if (type == 'RasterLayer') {
		r <- raster(xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, ncols=ncols, nrows=nrows, crs=crs)
	} else {
		r <- brick(xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymax, ncols=ncols, nrows=nrows, crs=crs)
	}
	r@file@name <- filename

	r@file@driver <- "netcdf"	
	r@data@source <- 'disk'

	varinfo <- try(var.inq.nc(nc, 'value'))
	datatype <- .getRasterDTypeFromCDF(varinfo$type)
	dims <- varinfo$ndims
	
	r@file@nbands <- as.integer(dim.inq.nc(nc, var.inq.nc(nc, 'value')$dimids[2])$length)
	if (is.na(band)) {
		r@file@band <- 1
	} else {
		r@file@band <- min(max(1, band), r@file@nbands)
	}

	r@data@min <- att.get.nc(nc, "value", 'min')
	r@data@max <- att.get.nc(nc, "value", 'max')

	close.nc(nc)
	return(r)
}

