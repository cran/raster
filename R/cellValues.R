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
	function(x, cells, layer=1, nlayers) { 
	
		layer = min( max( round(layer), 1), nlayers(x))
		if (missing(nlayers)) {
			nlayers = nlayers(x)-layer+1 
		} else {
			nlayers =  min( max( round(nlayers), 1), nlayers(x)-layer+1 )
		}

		result <- matrix(ncol=nlayers, nrow=length(cells))
		lyrs <- layer:(layer+nlayers-1)
		for (i in 1:nlayers) {
			j = lyrs[i]
			result[,i] <- .readCells( x@layers[[j]], cells )
		}
		if (!(is.null(dim(result)))) {
			colnames(result) <- layerNames(x)[layer:(layer+nlayers-1)]
		}
		result
	}
)

setMethod("cellValues", signature(x='RasterBrick', cells='vector'), 
function(x, cells, ...) {

	dots <- list(...)
	layer <- dots$layer
	nlayers <- dots$nlayers
	if (is.null(layer)) { layer <- 1 } 
	if (is.null(nlayers)) { nlayers <- x@data@nlayers } 
	
	nl <- x@data@nlayers 
	layer <- min(max(1, round(layer)), nl)
	maxnl = nl - layer + 1
	nlayers <- min(max(1, round(nlayers)), maxnl)

	if (x@file@driver == 'netcdf') {
		return( .readBrickCellsNetCDF(x, cells, layer, nlayers) )
	} 

	result <- matrix(nrow=length(cells), ncol=nlayers)
	lyrs <- layer:(layer+nlayers-1)
	for (i in 1:nlayers) {
		j <- lyrs[i]
		r <- raster(x, j)
		result[,i] <- .readCells(r, cells)
	}
	colnames(result) <- layerNames(x)[lyrs]
	return(result)
}
)
