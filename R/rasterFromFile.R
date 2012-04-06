# R raster package
# Date : September 2009
# Version 1.0
# Licence GPL v3


.requireRgdal <- function(stopIfAbsent=TRUE) {
	w <- getOption('warn')
	options('warn'=-1) 
	x <- isTRUE( try( require(rgdal, quietly=TRUE ) ) )
	options('warn'= w) 
	if (!x & stopIfAbsent) {
		stop("package 'rgdal' is not available")
	}
	return(x)
}


.rasterObjectFromFile <- function(x, band=1, objecttype='RasterLayer', native=FALSE, silent=TRUE, offset=NULL, ...) {
	x <- trim(x)
	if (x=='' | x=='.') { # etc? 
		stop('provide a valid filename')
	}

	# fix for opendap https://r-forge.r-project.org/forum/message.php?msg_id=5015
	start <- tolower(substr(x, 1, 4))
	if (start != 'http' & start != 'ftp') {	
		y <- NULL
		try( y <- normalizePath( x, mustWork=TRUE), silent=TRUE )
		if (! is.null(y)) {
			x <- y
		}
	}
	
	fileext <- toupper(extension(x)) 

	if (fileext %in% c(".GRD", ".GRI")) {
		grifile <- .setFileExtensionValues(x, 'raster')
		grdfile <- .setFileExtensionHeader(x, 'raster')
		if ( file.exists( grdfile) & file.exists( grifile)) {
			return ( .rasterFromRasterFile(grdfile, band=band, objecttype) )
		} 
	}
	if (! file.exists(x) ) {
		if (extension(x) == '') {
			grifile <- .setFileExtensionValues(x, 'raster')
			grdfile <- .setFileExtensionHeader(x, 'raster')
			if ( file.exists( grdfile) & file.exists( grifile)) {
				return ( .rasterFromRasterFile(grdfile, band=band, objecttype) )
			} else {
				# stop('file: ', x, ' does not exist')
			}
		}
	}

	if ( fileext %in% c(".NC", ".NCDF", ".NETCDF")) {
		return ( .rasterObjectFromCDF(x, type=objecttype, band=band, ...) )
		# return ( .rasterFromCDF(x, objecttype, ...) )
	}
	if ( fileext == ".GRD") {
		if (require(ncdf)) {
			if (.isNetCDF(x)) {
				# return ( .rasterFromCDF(x, objecttype, ...) )
				return ( .rasterObjectFromCDF(x, type=objecttype, band=band, ...) )
			}
		}
	}

	if (!is.null(offset)) {
		return ( .rasterFromASCIIFile(x, offset) )
	}
	if(!native) {
		if (! .requireRgdal(FALSE) )  { native <- TRUE }  
	}
	if (native) {
		if ( fileext == ".ASC" ) {
			return ( .rasterFromASCIIFile(x, ...) )
		}
		if ( fileext %in% c(".BIL", ".BIP", ".BSQ")) {
			return ( .rasterFromGenericFile(x, type=objecttype, ...) )
		}
		if ( fileext %in% c(".RST", ".RDC") ) {
#  not tested much
			return ( .rasterFromIDRISIFile(x) )
		}
		if ( fileext %in% c(".SGRD", ".SDAT") ) {
# barely tested
			return ( .rasterFromSAGAFile(x) )
		}
		
	}
	
	if ( fileext %in% c(".SGRD", ".SDAT") ) {
		r <-  .rasterFromSAGAFile(x) 
		if (r@file@toptobottom | r@data@gain != 1) {
			return(r)
		} # else use gdal
	}

	if (! .requireRgdal(FALSE) ) {
		stop("Cannot create RasterLayer object from this file; perhaps you need to install rgdal first")
	}
	test <- try( r <- .rasterFromGDAL(x, band=band, objecttype, ...), silent=silent )
	if (class(test) == "try-error") {
		if (!file.exists(x)) {
			stop("Cannot create a RasterLayer object from this file. (file does not exist)")
		}
		stop("Cannot create a RasterLayer object from this file.")
	} else {
		return(r)
	}
}

