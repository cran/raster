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
		if (missing(n)) { n = nlayers(x) }
		n =  min( max( round(n), 1), nlayers(x)-layer+1 )

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
function(x, cells, layer=1, n) {

	layer = min( max( round(layer), 1), nlayers(x))
	if (missing(n)) { n = nlayers(x) } 
	n =  min( max( round(n), 1), nlayers(x)-layer+1 )
	
		
	if (inMemory(x)) {
		cells[cells < 1 | cells > ncell(x)] <- NA
		if (length(na.omit(cells)) == 0) {
			return(cells)
		}
		return( x@data@values[cells, 1:n] )
	}
		
	if (x@file@driver == 'netcdf') {
		return( .readBrickCellsNetCDF(x, cells, layer, n) )
	} 

	result <- matrix(nrow=length(cells), ncol=n)
	lyrs <- layer:(layer+n-1)
	# this loop needs to be removed!
	for (i in 1:n) {
		j <- lyrs[i]
		r <- raster(x, j)
		result[,i] <- .readCells(r, cells)
	}
	colnames(result) <- layerNames(x)[lyrs]
	return(result)
}
)


