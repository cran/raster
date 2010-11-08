# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

 
writeHdr <- function(x, format) {

	if (inherits(x, 'RasterStack')) { stop('Only applicable to RasterLayer and RasterBrick classes (and their derivatives)') }
	if (x@file@name == '') { stop('Object has no filename') }

	type <- toupper(format)
	if (type=="RASTER") {
		.writeHdrRaster(x)
	} else if (type=="VRT") {
		.writeHdrVRT(x)
		.writeStx(x)		
	} else if (type=="BIL") {
		.writeHdrBIL(x)
		.writeStx(x)
	} else if (type=="BSQ") {
		.writeHdrBIL(x, "BSQ")
		.writeStx(x)
	} else if (type=="BIP") {
		.writeHdrBIL(x, "BIP")
		.writeStx(x)
	} else if (type=="ERDASRAW") {
		.writeHdrErdasRaw(x)
		.writeStx(x)
	} else 	if (type=="ENVI") {
		.writeHdrENVI(x)
		.writeStx(x)
	} else 	if (type=="SAGA") {
		.writeHdrSAGA(x)
	} else 	if (type=="IDRISI") {
		.writeHdrIDRISI(x)
	} else {
		stop("This file format is not supported")
	}
	return( invisible(TRUE) )
 }

 
 
.writeStx <- function(x, filename='') {
	if (x@data@haveminmax) {
		if (filename=='') {
			filename <- filename(x)
		} 
		if (filename!='') {
			ext(filename) <- ".stx"
			thefile <- file(filename, "w")  # open a txt file connectionis
			cat(1, " ", minValue(x), " ", maxValue(x), "\n", file = thefile)
			close(thefile)
		}
	}	
	return( invisible(TRUE) )
}
 