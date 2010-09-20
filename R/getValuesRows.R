# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

setMethod('getValues', signature(x='RasterStack', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)

setMethod('getValues', signature(x='RasterStack', row='numeric', nrows='numeric'), 
function(x, row, nrows) {
	for (i in 1:nlayers(x)) {
		if (i==1) {
			v <- getValues(x@layers[[i]], row, nrows)
			res <- matrix(ncol=nlayers(x), nrow=length(v))
			colnames(res) <- layerNames(x)
			res[,1] <- v
		} else {
			res[,i] <- getValues(x@layers[[i]], row, nrows)
		}
	}
	res
}
)

setMethod('getValues', signature(x='RasterLayer', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)

setMethod('getValues', signature(x='RasterLayer', row='numeric', nrows='numeric'), 
function(x, row, nrows) {

	if (! validRow(x, row)) { stop(row, ' is not a valid rownumber') }
	row <- min(x@nrows, max(1, round(row)))
	endrow <- max(min(x@nrows, row+round(nrows)-1), row)
	nrows <- endrow - row + 1
	
	if (inMemory(x)){
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- cellFromRowCol(x, row+nrows-1, x@ncols)
		return( x@data@values[startcell:endcell] )
	} else if ( fromDisk(x) ) {
		return( .readRasterLayerValues(x, row, nrows) )		
	} else {
		return( rep(NA, nrows * x@ncols) )
	}
}
)


setMethod('getValues', signature(x='RasterBrick', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)

setMethod('getValues', signature(x='RasterBrick', row='numeric', nrows='numeric'), 
function(x, row, nrows) {

	if (! validRow(x, row)) { stop(row, ' is not a valid rownumber') }
	row <- min(x@nrows, max(1, round(row)))
	endrow <- max(min(x@nrows, row+round(nrows)-1), row)
	nrows <- endrow - row + 1

	startcell <- cellFromRowCol(x, row, 1)
	endcell <- cellFromRowCol(x, row+nrows-1, x@ncols)

	readrow <- FALSE
	if (!  inMemory(x) ) {
		readrow <- TRUE
	} else if ( inMemory(x) ){
		res <- x@data@values[startcell:endcell,]
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
	if (readrow) {
		res <- .readRasterBrickValues(x, row, nrows)
	}
	colnames(res) <- layerNames(x)
	return(res)
}
)
