# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

 
writeRasterHdr <- function(raster, format) {
	type <- toupper(format)
	if (type=="RASTER") {
		.writeHdrRaster(raster)
	} else if (type=="BIL") {
		.writeHdrBIL(raster)
		.writeStx(raster)
	} else if (type=="BSQ") {
		.writeHdrBIL(raster, "BSQ")
		.writeStx(raster)
	} else if (type=="BIP") {
		.writeHdrBIL(raster, "BIP")
		.writeStx(raster)
	} else if (type=="ERDASRAW") {
		.writeHdrErdasRaw(raster)
		.writeStx(raster)
	} else 	if (type=="ENVI") {
		.writeHdrENVI(raster)
		.writeStx(raster)
	} else 	if (type=="SAGA") {
		.writeHdrSAGA(raster)
	} else 	if (type=="IDRISI") {
		.writeHdrIDRISI(raster)
	} else {
		stop("This file format is not supported")
	}
 }

 
 
.writeStx <- function(raster, filename) {
	if (raster@data@haveminmax) {
		if (missing(filename)) {
			filename <- filename(raster)
		} 
		ext(filename) <- ".stx"
		thefile <- file(filename, "w")  # open a txt file connectionis
		cat(1, " ", minValue(raster), " ", maxValue(raster), "\n", file = thefile)
		close(thefile)
	}	
}
 