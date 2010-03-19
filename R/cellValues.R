# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.9
# Licence GPL v3


###   cellValues   ###

if (!isGeneric("cellValues")) {
	setGeneric("cellValues", function(x, cells)
		standardGeneric("cellValues"))
}

	
setMethod("cellValues", signature(x='RasterLayer', cells='vector'), 
	function(x, cells) { 
		return(.readCells(x, cells))
	}
)

setMethod("cellValues", signature(x='RasterBrick', cells='vector'), 
	function(x, cells) { 
		return(.brickReadCells(x, cells))
	}
)


setMethod("cellValues", signature(x='RasterStack', cells='vector'), 
	function(x, cells) { 
		result <- matrix(ncol=nlayers(x), nrow=length(cells))
		for (i in seq(nlayers(x))) {
			result[,i] <- .readCells( x@layers[[i]], cells )
		}
		if (!(is.null(dim(result)))) {
			colnames(result) <- layerNames(x)
		}	
		result
	}
)

