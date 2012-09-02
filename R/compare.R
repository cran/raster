# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


compare <- function(x, ..., extent=TRUE, rowcol=TRUE, crs=TRUE, res=FALSE, orig=FALSE, rotation=TRUE, tolerance, stopiffalse=TRUE, showwarning=FALSE) {

	if (missing(tolerance)) {
		tolerance <- .tolerance()
	}
	
	result <- TRUE
	objects <- c(x, list(...))
	if (!isTRUE(length(objects) > 1)) {
		warning('There should be at least 2 Raster* objects to compare')
		return(result)
	}	
	minres <- min(res(objects[[1]]))
	proj1 <- projection(objects[[1]])
	ext1 <- extent(objects[[1]])
	ncol1 <- ncol(objects[[1]])
	nrow1 <- nrow(objects[[1]])
	res1 <- res(objects[[1]])
	origin1 <- abs(origin(objects[[1]]))
	rot1 <- rotated(objects[[1]])	
	
	for (i in 2:length(objects)) { 
		if (extent) {
			if (!(isTRUE(all.equal(ext1, extent(objects[[i]]), tolerance=tolerance, scale=minres )))) {
				result <- FALSE
				if (stopiffalse) { stop('Different extent') }
				if (showwarning) { warning('Different extent') }
			}	
		}	
		if (rowcol) {
			if ( !(identical(ncol1, ncol(objects[[i]]))) ) {
				result <- FALSE
				if (stopiffalse) { stop('ncols different') } 
				if (showwarning) { warning('ncols different') } 
			}	
			if ( !(identical(nrow1, nrow(objects[[i]]))) ) {
				result <- FALSE
				if (stopiffalse) { stop('nrows different') }
				if (showwarning) { warning('nrows different') }
			}
		}
		if (crs) {
			thisproj <- projection(objects[[i]])
			if (is.na(proj1)) {
				proj1 <- thisproj
			} else if ( ! .compareCRS(proj1, thisproj, unknown=TRUE) ) {
				result <- FALSE
				if (stopiffalse) { stop('different projection') }
				if (showwarning) { warning('different projection') }
			}
		}
		
# Can also check res through extent & rowcol
		if (res) {
			if (!(isTRUE(all.equal(res1, res(objects[[i]]), tolerance=tolerance, scale=minres)))) {
				result <- FALSE
				if (stopiffalse)  { stop('different resolution') }
				if (showwarning) { warning('different resolution') }
			}	
		}
# Can also check orig through extent & rowcol, but orig is useful for e.g. Merge(raster, raster)
		if (orig) {
			dif <- origin1 - abs(origin(objects[[i]]))
			if (!(isTRUE(all.equal(dif, c(0,0), tolerance=tolerance, scale=minres)))) {
				result <- FALSE
				if (stopiffalse) { stop('different origin') }
				if (showwarning) { warning('different origin') }
			}
		}
		
		if (rotation) {
			rot2 <- rotated(objects[[i]])
			if (rot1 | rot2) {
				if (rot1 != rot2) {
					if (stopiffalse) { stop('not all objects are rotated') }
					if (showwarning) { warning('not all objects are rotated') }
					result <- FALSE
				} else {
					test <- all(objects[[i]]@rotation@geotrans == objects[[1]]@rotation@geotrans)
					if (! test) {
						if (stopiffalse) { stop('rotations are different') }
						if (showwarning) { warning('rotations are different') }
						result <- FALSE
					}
				}
			}
		}
	}
	return(result)
}

