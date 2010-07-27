# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date : June 2008
# Version 0.9
# Licence GPL v3


closeConnection <- function(raster) {
	if (.driver(raster) == "gdal") {
		try(closeDataset(raster@file@con), silent = T)
	} else {
		cr <- try(close(raster@file@con), silent = T)
	}
	attr(raster@file, "con" <- "")
	return(raster)
}
