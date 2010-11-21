# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 1.0
# Licence GPL v3

cellValues <- function(x, cells, ...) { 
	.warnExtract()
	extract(x, cells, ...)
}

	
.cellValues <- function(x, cells, layer, nl) { 

	if (inherits(x, 'RasterLayer')) {
		return( .readCells(x, cells) )
		
	} else {
	
		nlyrs <- nlayers(x)
		if (missing(layer)) { layer <- 1 }
		layer <- min( max( round(layer), 1), nlyrs)
		if (missing(nl)) { nl <- nlayers(x) }
		nl <-  min( max( round(nl), 1), nlyrs-layer+1 )
		lyrs <- layer:(layer+nl-1)
	
		if (inherits(x, 'RasterStack')) {
		
			result <- matrix(ncol=nl, nrow=length(cells))
		
			for (i in 1:nl) {
				j = lyrs[i]
				result[,i] <- .readCells( x@layers[[j]], cells )
			}
			
		} else if (inherits(x, 'RasterBrick')) {
		
			if (inMemory(x)) {
				cells[cells < 1 | cells > ncell(x)] <- NA
				if (length(na.omit(cells)) == 0) {
					return(cells)
				}
				return( x@data@values[cells, lyrs] )
			} 
		
			if (x@file@driver == 'netcdf') {
				return( .readBrickCellsNetCDF(x, cells, layer, nl) )
			} 

			result <- matrix(nrow=length(cells), ncol=nl)
			lyrs <- layer:(layer+nl-1)
	# this loop needs to be removed!
			for (i in 1:nl) {
				j <- lyrs[i]
				r <- raster(x, j)
				result[,i] <- .readCells(r, cells)
			}
		}
		
		colnames(result) <- layerNames(x)[lyrs]
		return( result )
		
	}	
}

