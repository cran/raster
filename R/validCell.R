# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  October 2008
# Version 0.9
# Licence GPL v3


validCell <- function(object, cell) {
	object <- raster(object)
	cell <- round(cell)
	validcell <- vector(length=length(cell))
	validcell[cell > 0 & cell <= ncell(object)] <- TRUE
	return(validcell)
}

validRow <- function(object, rownr) {
	object <- raster(object)
	rownr <- round(rownr)
	validrows <- vector(length=length(rownr))
	validrows[rownr > 0 & rownr <= nrow(object)] <- TRUE
	return(validrows)
}

validCol <- function(object, colnr) {
	object <- raster(object)
	colnr <- round(colnr)
	validcols <- vector(length=length(colnr))
	validcols[colnr > 0 & colnr <= nrow(object)] <- TRUE
	return(validcols)
}
