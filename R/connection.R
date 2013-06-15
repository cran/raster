# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("readStart")) {
	setGeneric("readStart", function(x, ...)
		standardGeneric("readStart"))
}	

setMethod('readStart', signature(x='Raster'), 
	function(x, ...) {
		if ( fromDisk(x) ) {
			return (.openConnection(x, ...))
		} else {
			return(x)
		}
	}
)


setMethod('readStart', signature(x='RasterStack'), 
	function(x, ...) {
		for (i in 1:nlayers(x)) {
			x@layers[[i]] <- readStart(x@layers[[i]], con.check=103, ...)
		}
		x
	}
)




.openConnection <- function(x, silent=TRUE, con.check=Inf, ...) {
	fn <- trim(x@file@name)
	driver <- .driver(x)
	if (driver == "gdal") {
		attr(x@file, "con") <- GDAL.open(fn, silent=silent)
		x@file@open <- TRUE
	} else 	if (.isNativeDriver(driver))  {
		# R has a max of 128 connections
		if (length(getAllConnections()) < con.check) {
			fn <- .setFileExtensionValues(fn, driver)
			attr(x@file, "con") <- file(fn, "rb")
			x@file@open <- TRUE
		}
	} else if (driver == 'netcdf') {
		if (isTRUE(getOption('rasterNCDF4'))) {
			attr(x@file, 'con') <- ncdf4::nc_open(x@file@name)
		} else {
			attr(x@file, 'con') <- open.ncdf(x@file@name)
		}
		x@file@open <- TRUE
#	} else if (driver == 'ascii') { # cannot be opened
	}	
	x
}




if (!isGeneric("readStop")) {
	setGeneric("readStop", function(x, ...)
		standardGeneric("readStop"))
}	

setMethod('readStop', signature(x='Raster'), 
	function(x, ...) {
		if ( fromDisk(x) ) {
			return (.closeConnection(x))
		} else {
			return(x)
		}
	}
)

setMethod('readStop', signature(x='RasterStack'), 
	function(x, ...) {
		for (i in 1:nlayers(x)) {
			x@layers[[i]] <- readStop(x@layers[[i]], ...)
		}
		x
	}
)


.closeConnection <- function(x) {
	driver <- .driver(x)
	if (driver == "gdal") {
		try( closeDataset(x@file@con), silent = TRUE )
	} else if (.isNativeDriver(driver))  {
		try( close(x@file@con), silent = TRUE )
	} else if (driver == 'netcdf') {
		if (isTRUE(getOption('rasterNCDF4'))) {
			ncdf4::nc_close(x@file@con)
		} else {
			close.ncdf(x@file@con)
		}	
	} else if (driver == 'ascii') {	}
	
	x@file@open <- FALSE
	attr(x@file, 'con') <- NULL
	x
#	attr(x@file, "con" <- "")
}

