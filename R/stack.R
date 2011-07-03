# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("stack")) {
	setGeneric("stack", function(x, ...)
		standardGeneric("stack"))
}	

setMethod("stack", signature(x='missing'), 
function(x) {
	return(new("RasterStack"))
	}
)

setMethod("stack", signature(x='Raster'), 
function(x, ...) {
	rlist <- .makeRasterList(x, ...)
	return( stack(rlist) )
	}
)



setMethod("stack", signature(x='character'), 
function(x, ..., bands=NULL, varname="") {
	if (!is.null(bands)) {
		if (length(list(...)) > 0) {
			stop("if you supply a 'bands' argument, you can only supply a single filename")
		}
	}
    if ( varname != "") {
		return(.stackCDF(x, varname=varname, bands=bands))
	} else {
		rlist <- c(x, list(...))
		return(stack(rlist, bands=bands))
	}
} )


setMethod("stack", signature(x='list'), 
function(x, bands=NULL, ...) {
	if (class(x) == 'data.frame') {
		return(utils::stack(x, ...))
	}
	j <- 0
	r <- list()
	lstnames <- names(x)
	if (is.null(lstnames)) {
		namesFromList <- FALSE
	} else {
		lstnames <- trim(lstnames)
		namesFromList <- TRUE
	}

	if (is.character(x[[1]]) & length(x) == 1 & !is.null(bands)) {
		first <- raster(x[[1]])
		lb <- length(bands)
		bands <- bands[bands %in% 1:nbands(first)]
		if (length(bands) == 0) {
			stop('no valid bands supplied')
		}
		if (length(bands) < lb) {
			warning('invalid band numbers ignored')
		}
		for (b in bands) {
			r[b] <- raster(x[[1]], band=b)
			if (namesFromList) {
				if (lstnames != "") {
					layerNames(r[[b]]) <- lstnames
				}
			}
		}
		
	} else {
	
		for (i in seq(along=x)) {
			j <- j + 1
			if (is.character(x[[i]])) {

				r[j] <- raster(x[[i]], band=1)
				if (namesFromList) {
					if (lstnames[i] != "") {
						layerNames(r[[j]]) <- lstnames[i]
					}
				}

				bds <- nbands(r[[j]])
				if (bds > 1) {
					for (b in 2:bds) {
						j <- j + 1
						r[j] <- raster(x[[i]], band=b)
						
						if (namesFromList) {
							if (lstnames[i] != "") {
								layerNames(r[[j]]) <- lstnames[i]
							}
						}

					}
				}
			} else if (extends(class(x[[i]]), "Raster")) {
				r[j] <- x[[i]]
				if (namesFromList) {
					if (lstnames[i] != "") {
						layerNames(r[[j]]) <- lstnames[i]
					}
				}
			} else {
				stop("Arguments should be Raster* objects or filenames")	
			}
		}
	}
	
	if ( length(r) == 1 ) {
		r <- r[[1]]
		if ( hasValues(r) ) {
			return( addLayer( new("RasterStack"), r) )
		} else {
			x <- new("RasterStack")
			x@nrows <- r@nrows
			x@ncols <- r@ncols
			x@extent <- r@extent
			x@crs <- r@crs
			if(rotated(r)) {
				x@rotated = r@rotated
				x@rotation = r@rotation
			}
			return(x)
		}
	} else {
		return(addLayer(new("RasterStack"), r))
	}
}
)


setMethod("stack", signature(x='SpatialGridDataFrame'), 
	function(x) {
		stk <- new("RasterStack")
		for (i in 1:ncol(x@data)) {
			rs <- raster(x, i)
			stk <- addLayer(stk, rs)
		}
		return(stk)
	}
)
	

setMethod("stack", signature(x='SpatialPixelsDataFrame'), 
	function(x) {
		x <- as(x, 'SpatialGridDataFrame')
		return(stack(x))
	}
)


setMethod('stack', signature(x='kasc'), 
	function(x) {
		as(x, 'RasterStack')
	}
)
