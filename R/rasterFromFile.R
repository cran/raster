# R raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3



.rasterObjectFromFile <- function(x, band=1, objecttype='RasterLayer', native=FALSE, ...) {
	x <- trim(x)
	if (x=='' | x=='.') { # etc? 
		stop('provide a valid filename')
	}
	fileext <- toupper(ext(x)) 

	if (fileext %in% c(".GRD", ".GRI")) {
		grifile <- .setFileExtensionValues(x, 'raster')
		grdfile <- .setFileExtensionHeader(x, 'raster')
		if ( file.exists( grdfile) & file.exists( grifile)) {
			return ( .rasterFromRasterFile(grdfile, band, objecttype) )
		} 
	}
	if (! file.exists(x)) {
		grifile <- .setFileExtensionValues(x, 'raster')
		grdfile <- .setFileExtensionHeader(x, 'raster')
		if ( file.exists( grdfile) & file.exists( grifile)) {
			return ( .rasterFromRasterFile(grdfile, band, objecttype) )
		} else {
			stop('file: ', x, ' does not exist')
		}
	}

	if ( fileext %in% c(".SGRD", ".SDAT") ) {
# barely tested
		return ( .rasterFromSAGAFile(x) )
	}
	if ( fileext %in% c(".NC", ".NCDF", ".NETCDF")) {
		return ( .rasterFromCDF(x, objecttype, ...) )
	}
	if ( fileext == ".GRD") {
		if (require(RNetCDF)) {
			if (.isNetCDF(x)) {
				return ( .rasterFromCDF(x, objecttype, ...) )
			}
		}
	}

	if(!native) {
		if (!(require(rgdal))) { native <- TRUE }  
	}
	if (native) {
		if ( fileext == ".ASC" ) {
			return ( .rasterFromASCIIFile(x) )
		}
		if ( fileext %in% c(".BIL", ".BIP", ".BSQ")) {
			return ( .rasterFromGenericFile(x, ...) )
		}
		if ( fileext %in% c(".RST", ".RDC") ) {
#  not tested much
			return ( .rasterFromIDRISIFile(x) )
		}
	}
	if (!require(rgdal)) {
		stop("Cannot create RasterLayer object from this file; perhaps you need to install rgdal first")
	}
	test <- try ( r <- .rasterFromGDAL(x, band, objecttype), silent=TRUE )
	if (class(test) == "try-error") {
		stop("Cannot create a RasterLayer object from this file.")
	} else {
		return(r)
	}
}

