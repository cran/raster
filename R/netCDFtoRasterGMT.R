# Author: Robert J. Hijmans
# Date: March 2013
# Version 1.0
# Licence GPL v3

.rasterObjectFromCDF_GMT <- function(nc, ncdf4) {
#	ncdf4 <- .NCDFversion4()
	if (ncdf4) {
		options(rasterNCDF4 = TRUE)
#		nc <- ncdf4::nc_open(filename)
#		on.exit( ncdf4::nc_close(nc) )		
	#	conv <- ncdf4::ncatt_get(nc, 0, "Conventions")
		dims <- ncdf4::ncvar_get(nc, "dimension", 1)
		xr <- ncdf4::ncvar_get(nc, "x_range", 1)
		yr <- ncdf4::ncvar_get(nc, "y_range", 1)
		zr <- ncdf4::ncvar_get(nc, "z_range", 1)
		sp <- ncdf4::ncvar_get(nc, "spacing", 1)
		
	} else {
		options(rasterNCDF4 = FALSE)
#		nc <- ncdf::open.ncdf(filename)
#		on.exit( ncdf::close.ncdf(nc) )		
	#	conv <- ncdf::att.get.ncdf(nc, 0, "Conventions")
		dims <- ncdf::get.var.ncdf(nc, "dimension", 1)
		xr <- ncdf::get.var.ncdf(nc, "x_range", 1)
		yr <- ncdf::get.var.ncdf(nc, "y_range", 1)
		zr <- ncdf::get.var.ncdf(nc, "z_range", 1)
		sp <- ncdf::get.var.ncdf(nc, "spacing", 1)
	} 
	zvar = 'z'
	#datatype <- .getRasterDTypeFromCDF( nc$var[[zvar]]$prec )
	#ncell <- nc$var[[zvar]]$dim[[1]]$len
	#stopifnot(prod(dims) == ncell)

	crs <- NA
	if (xr[1] > -181 & xr[2] < 181 & yr[1] > -91 & yr[2] < 91 ) {
		crs <- "+proj=longlat +datum=WGS84"
	}

	dif1 <- abs(((xr[2] - xr[1]) / dims[1]) - sp[2])
	dif2 <- abs(((xr[2] - xr[1]) / (dims[1]-1)) - sp[2])
	
	if (dif1 < dif2) {  # 30 sec GEBCO data
		r <- raster(xmn=xr[1], xmx=xr[2], ymn=yr[1], ymx=yr[2], ncol=dims[1], nrow=dims[2], crs=crs)
	} else {  # 1 min data 
		resx <- (xr[2] - xr[1]) / (dims[1]-1)
		resy <- (yr[2] - yr[1]) / (dims[2]-1)
		r <- raster(xmn=xr[1]-(0.5*resx), xmx=xr[2]+(0.5*resx), ymn=yr[1]-(0.5*resy), ymx=yr[2]+(0.5*resy), ncol=dims[1], nrow=dims[2], crs=crs)
	}
	
	r@file@name <- nc$filename
	r@file@toptobottom <- FALSE
	attr(r@data, "zvar") <- zvar
	attr(r@data, "dim3") <- 1
	r@file@driver <- "netcdf"
	r@data@fromdisk <- TRUE
	return(r)
}

