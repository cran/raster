# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


.splitProj <- function(x) {
	x <- trim(unlist(strsplit(x, '\\+')))
	x <- x[x!=""]
#	x <- gsub('  ', ' ', x)
#	x <- strsplit(x, ' ')
#	x <- unlist(lapply(x, function(x)strsplit(x, '=')))
#	x <- matrix(x, ncol=2, byrow=T)
}

.compareProj <- function(x, y) {
# needs work, parse out elements and compare these
# "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" is the same as 
# "+proj=longlat +ellps=WGS84 +datum=WGS84"

	x <- substr(x, 1, 10)
	y <- substr(y, 1, 10)
	return(identical(x, y))
}


compare <- function(x, ..., extent=TRUE, rowcol=TRUE, prj=TRUE, res=FALSE, orig=FALSE, tolerance, stopiffalse=TRUE, showwarning=FALSE) {

	if (missing(tolerance)) {
		tolerance <- options("rasterTolerance")$rasterTolerance
		if (is.null(tolerance)) {
			tolerance <- 0.05
		}
	}
	result <- TRUE
	firstproj <- NA
	objects <- c(x, list(...))
	if (!isTRUE(length(objects) > 1)) {
		warning('There should be at least 2 Raster* objects to compare')
		return(result)
	}	
	minres <- min(res(objects[[1]]))
	for (i in 2:length(objects)) { 
		if (extent) {
			if (!(isTRUE(all.equal(extent(objects[[1]]), extent(objects[[i]]), tolerance=tolerance, scale=minres )))) {
				result <- FALSE
				if (stopiffalse) { stop('Different extent') }
				if (showwarning) { warning('Different extent') }
			}	
		}	
		if (rowcol) {
			if ( !(identical(ncol(objects[[1]]), ncol(objects[[i]]))) ) {
				result <- FALSE
				if (stopiffalse) { stop('ncols different') } 
				if (showwarning) { warning('ncols different') } 
			}	
			if ( !(identical(nrow(objects[[1]]), nrow(objects[[i]]))) ) {
				result <- FALSE
				if (stopiffalse) { stop('nrows different') }
				if (showwarning) { warning('nrows different') }
			}
		}
		if (prj) {
			if ( is.na( projection(objects[[i]] )) ) {
				# skip
			} else if (is.na(firstproj)) {
				firstproj <- substr(projection(objects[[i]]), 1, 10)
			} else if ( ! identical(firstproj, substr(projection(objects[[i]]), 1, 10)))  {
				result <- FALSE
				if (stopiffalse) { stop('different projection') }
				if (showwarning) { warning('different projection') }
			}
		}
# Can also check res through extent & rowcol
		if (res) {
			if (!(isTRUE(all.equal(res(objects[[1]]), res(objects[[i]]), tolerance=tolerance, scale=minres)))) {
				result <- FALSE
				if (stopiffalse)  { stop('different resolution') }
				if (showwarning) { warning('different resolution') }
			}	
		}
# Can also check orig through extent & rowcol, but orig is useful for e.g. Merge(raster, raster)
		if (orig) {
			dif <- abs(origin(objects[[i]])) - abs(origin(objects[[1]]))
			if (!(isTRUE(all.equal(dif, c(0,0), tolerance=tolerance, scale=minres)))) {
				result <- FALSE
				if (stopiffalse) { stop('different origin') }
				if (showwarning) { warning('different origin') }
			}
		}
	}
	return(result)
}
