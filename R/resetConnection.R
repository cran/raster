# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : April  2009
# Version 0.9
# Licence GPL v3



openConnection <- function(raster, silent=FALSE) {
	fn <- trim(filename(raster))
	driver <- .driver(raster)
	if (driver == "gdal") {
		attr(raster@file, "con") <- GDAL.open(fn, silent=silent)
	} else {
		fn <- .setFileExtensionValues(fn, driver)
		attr(raster@file, "con") <- file(fn, "rb")
	} 
	return(raster)
}


