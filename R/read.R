# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("readAll")) {
	setGeneric("readAll", function(object)
		standardGeneric("readAll"))
}

	
setMethod('readAll', signature(object='RasterLayer'), 
	function(object){ 
		.readRasterLayerValues(object, 1, object@nrows)
	}
)


setMethod('readAll', signature(object='RasterStack'), 
	function(object){ 
		for (i in seq(nlayers(object))) {
			object@layers[[i]] <- .readRasterLayerValues(object@layers[[i]], 1, object@nrows)
		}
		object
	}
)


setMethod('readAll', signature(object='RasterBrick'), 
	function(object){ 
		.readRasterBrickValues(object, 1, object@nrows)
	}
)



if (!isGeneric("..readRow")) {
	setGeneric("..readRow", function(object, rownr)
		standardGeneric("..readRow"))
}

setMethod('..readRow', signature(object='RasterLayer'), 
	function(object, rownr){ 
		.readRasterLayerValues(object, rownr, 1)
	}
)

setMethod('..readRow', signature(object='RasterStack'), 
	function(object, rownr) { 
		for (i in seq(nlayers(object))) {
			object@layers[[i]] <- .readRasterLayerValues(object@layers[[i]], rownr, 1)
		}
		object
	}
)


setMethod('..readRow', signature(object='RasterBrick'), 
	function(object, rownr){ 
		.readRasterBrickValues(object, rownr, 1)
	}
)


	
if (!isGeneric("..readRows")) {
	setGeneric("..readRows", function(object, startrow, nrows)
		standardGeneric("..readRows"))
}	

setMethod('..readRows', signature(object='RasterLayer'), 
	function(object, startrow, nrows) { 
		.readRasterLayerValues(object, startrow, nrows)
	}
)

setMethod('..readRows', signature(object='RasterStack'), 
	function(object, startrow, nrows) { 
		for (i in seq(nlayers(object))) {
			object@layers[[i]] <- .readRasterLayerValues(object@layers[[i]], startrow, nrows)
		}
		object
	}
)

setMethod('..readRows', signature(object='RasterBrick'), 
	function(object, startrow, nrows) { 
		.readRasterBrickValues(object, startrow, nrows)
	}
)



if (!isGeneric("..readBlock")) {
	setGeneric("..readBlock", function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1))
		standardGeneric("..readBlock"))
}	

setMethod('..readBlock', signature(object='RasterLayer'), 
	function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		.readRasterLayerValues(object, startrow, nrows, startcol, ncolumns)
	}
)

setMethod('..readBlock', signature(object='RasterStack'), 
	function(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) { 
		for (i in seq(nlayers(object))) {
			object@layers[[i]] <- .readRasterLayerValues(object@layers[[i]], startrow, nrows, startcol, ncolumns)
		}
		object
	}
)

setMethod('..readBlock', signature(object='RasterBrick'), 
	function(object, startrow, nrows=3, startcol=1,  ncolumns=(ncol(object)-startcol+1)) { 
		.readRasterBrickValues(object, startrow, nrows, startcol, ncolumns)
	}
)


