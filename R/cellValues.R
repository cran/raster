# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 1.0
# Licence GPL v3

	
.cellValues <- function(x, cells, layer, nl) { 

	if (inherits(x, 'RasterLayer')) {
		return( .readCells(x, cells, 1) )
		
	} else {
	
		nlyrs <- nlayers(x)
		if (missing(layer)) { layer <- 1 }
		layer <- min( max( round(layer), 1), nlyrs)
		if (missing(nl)) { nl <- nlyrs }
		nl <-  min( max( round(nl), 1), nlyrs-layer+1 )
		lyrs <- layer:(layer+nl-1)
	
		if (inherits(x, 'RasterStack')) {
	
			result <- matrix(ncol=nl, nrow=length(cells))
			colnames(result) <- layerNames(x)[lyrs]
			for (i in 1:length(lyrs)) {
				result[,i] <- .readCells( x@layers[[lyrs[i]]], cells, 1)
			}
			return( result )
			
		} else if (inherits(x, 'RasterBrick')) {
		
			if (inMemory(x)) {
				cells[cells < 1 | cells > ncell(x)] <- NA
				if (length(na.omit(cells)) == 0) {
					return(cells)
				}
				return( x@data@values[cells, lyrs] )
				
			} else if (x@file@driver == 'netcdf') {
				return( .readBrickCellsNetCDF(x, cells, layer, nl) )
				
			}  else {
				result <-  .readCells(x, cells, lyrs) 
				if (is.null(dim(result))) { 
						# single layer of brick returns vector perhaps should be fixed in readCells
						# only an issue with a single layer?
					names(result) <- layerNames(x)[lyrs]
				}
				return(result)
			}
		}
	}
}	


