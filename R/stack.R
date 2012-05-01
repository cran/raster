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
function(x, ..., bands=NULL, varname="", native=FALSE, quick=FALSE) {
	rlist <- c(x, list(...))
    if ( varname != "") {
		if (length(rlist) == 1) {
			return(.stackCDF(x, varname=varname, bands=bands))
		} else {
			s <- stack(sapply(rlist, function(x) .stackCDF(x, varname=varname, bands=bands)))
		}
	} else {
		if (quick) {
			if (!is.null(bands)) {
				stop("cannot do 'quick' if bands is not NULL")
			}
			return(.quickStack(rlist, native=native))
		}
		return(stack(rlist, bands=bands, native=native))
	}
} )


setMethod("stack", signature(x='list'), 
function(x, bands=NULL, native=FALSE, ...) {
	if (class(x) == 'data.frame') {
		return(utils::stack(x, ...))
	}
	r <- list()
	lstnames <- names(x)
	if (is.null(lstnames)) {
		namesFromList <- FALSE
	} else {
		lstnames <- .goodNames(lstnames)
		namesFromList <- TRUE
	}

	first <- raster(x[[1]])
	if (!is.null(bands)) {
		lb <- length(bands)
		bands <- bands[bands %in% 1:nbands(first)]
		if (length(bands) == 0) {
			stop('no valid bands supplied')
		}
		if (length(bands) < lb) {
			warning('invalid band numbers ignored')
		}
	} 

	j <- 1
	for (i in seq(along=x)) {
		if (is.character(x[[i]])) {
			if (!is.null(bands)) {
				for (b in bands) {
					r[j] <- raster(x[[i]], band=b, native=native, ...)
					if (namesFromList) {
						layerNames(r[[j]]) <- paste(lstnames[i], '_', b, sep='')
					}
					j <- j + 1
				}
			} else {
				r[j] <- raster(x[[i]], band=1, native=native, ...)
				bds <- nbands(r[[j]])

				if (namesFromList) {
					if (bds > 1) {
						layerNames(r[[j]]) <- paste(lstnames[i], '_1', sep='')						
					} else {
						layerNames(r[[j]]) <- lstnames[i]
					}
				}
				j <- j + 1
				if (bds > 1) {
					for (b in 2:bds) {
						r[j] <- raster(x[[i]], band=b, native=native, ...)
							
						if (namesFromList) {
							layerNames(r[[j]]) <- paste(lstnames[i], '_', b, sep='')
						}
						j <- j + 1
					}
				}
			}
		} else if (extends(class(x[[i]]), "Raster")) {
			if (inherits(x[[i]], 'RasterStackBrick')) {
				if (!is.null(bands)) {
					for (b in bands) {
						r[j] <- raster(x[[i]], b)
						j <- j + 1
					}
				} else {
					if (inherits(x[[i]], 'RasterBrick')) {
						x[[i]] <- stack(x[[i]])
					}
					r <- c(r, x[[i]]@layers)
					j <- j + nlayers(x[[i]])
				}
			} else {
				r[j] <- x[[i]]
				if (namesFromList) {
					layerNames(r[[j]]) <- lstnames[i]
				}
				j <- j + 1				
			}
		} else {
			stop("Arguments should be Raster* objects or filenames")	
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
			x@layernames <- r@layernames
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
