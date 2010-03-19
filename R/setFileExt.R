# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.setFileExtensionValues <- function(fname, type='raster') {
	if (type == 'raster') {
		ext(fname) <- ".gri"
	} else if (type == 'SAGA') {
		ext(fname) <- ".sdat"
	} else if (type == 'IDRISI') {
		ext(fname) <- ".rst"
	} else if (type == 'BIL') {
		ext(fname) <- ".bil"
	} else if (type == 'BIP') {
		ext(fname) <- ".bip"
	} else if (type == 'BSQ') {
		ext(fname) <- ".bsq"
	} else {
		stop('unknown file format')
	}
	return(fname)
}
 
.setFileExtensionHeader <- function(fname, type='raster') {
	if (type == 'raster') {
		ext(fname) <- ".grd"
	} else if (type == 'SAGA') {
		ext(fname) <- "sgrd"
	} else if (type == 'IDRISI') {
		ext(fname) <- ".rdc"
	} else if (type %in% c('BIL', 'BSQ', 'BIP')) {
		ext(fname) <- ".hdr"
	} else {
		stop('unknown file format')
	}
	return(fname)
}
 