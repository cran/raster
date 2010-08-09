# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.9
# Licence GPL v3


###   cellValues   ###

if (!isGeneric("cellValues")) {
	setGeneric("cellValues", function(x, cells, ...)
		standardGeneric("cellValues")
	)
}

	
setMethod("cellValues", signature(x='RasterLayer', cells='vector'), 
	function(x, cells) { 
		return(.readCells(x, cells))
	}
)



setMethod("cellValues", signature(x='RasterStack', cells='vector'), 
	function(x, cells, layer=1, n) { 
	
		layer = min( max( round(layer), 1), nlayers(x))
		if (missing(n)) {
			n = nlayers(x)-layer+1 
		} else {
			n =  min( max( round(n), 1), nlayers(x)-layer+1 )
		}

		result <- matrix(ncol=n, nrow=length(cells))
		lyrs <- layer:(layer+n-1)
		for (i in 1:n) {
			j = lyrs[i]
			result[,i] <- .readCells( x@layers[[j]], cells )
		}
		if (!(is.null(dim(result)))) {
			colnames(result) <- layerNames(x)[layer:(layer+n-1)]
		}
		result
	}
)

setMethod("cellValues", signature(x='RasterBrick', cells='vector'), 
function(x, cells, ...) {

	dots <- list(...)
	layer <- dots$layer
	n <- dots$n
	if (is.null(layer)) { layer <- 1 } 
	if (is.null(n)) { n <- x@data@nlayers } 
	
	nl <- x@data@nlayers 
	layer <- min(max(1, round(layer)), nl)
	maxnl = nl - layer + 1
	n <- min(max(1, round(n)), maxnl)

	if (x@file@driver == 'netcdf') {
		return( .readBrickCellsNetCDF(x, cells, layer, n) )
	} 

	result <- matrix(nrow=length(cells), ncol=n)
	lyrs <- layer:(layer+n-1)
	for (i in 1:n) {
		j <- lyrs[i]
		r <- raster(x, j)
		result[,i] <- .readCells(r, cells)
	}
	colnames(result) <- layerNames(x)[lyrs]
	return(result)
}
)
