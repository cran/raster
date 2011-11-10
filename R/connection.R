# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


closeConnection <- function(x) {
	if (.driver(x) == "gdal") {
		try( closeDataset(x@file@con), silent = TRUE )
	} else {
		try( close(x@file@con), silent = TRUE )
	}
	attr(x@file, "con" <- "")
	return(x)
}


openConnection <- function(x, silent=FALSE) {
	fn <- trim(filename(x))
	driver <- .driver(x)
	if (driver == "gdal") {
		attr(x@file, "con") <- GDAL.open(fn, silent=silent)
	} else {
		fn <- .setFileExtensionValues(fn, driver)
		attr(x@file, "con") <- file(fn, "rb")
	} 
	return(x)
}


