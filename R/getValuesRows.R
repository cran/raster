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
	if (!is.atomic(row)) {	stop() }
	if (!is.atomic(nrows)) { stop() }
	row <- as.integer(round(row))
	nrows <- as.integer(round(nrows))
	readrow <- FALSE
	startcell <- cellFromRowCol(x, row, 1)
	endcell <- cellFromRowCol(x, row+nrows-1, x@ncols)

	if (!(validRow(x, row))) {	stop(paste(row, 'is not a valid rownumber')) }
	
	if (dataContent(x) == 'nodata') {
		readrow <- TRUE
	} else if (dataContent(x) == 'all'){
		res <- x@data@values[startcell:endcell]
	} else if (dataContent(x) == 'row') {
		if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
			res <- (x@data@values)
		} else {
			readrow <- TRUE
		}
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
	
	if (readrow) {	res <- ..readRows(x, row, nrows)@data@values }
	res
}
)

setMethod('getValues', signature(x='RasterBrick', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)

setMethod('getValues', signature(x='RasterBrick', row='numeric', nrows='numeric'), 
function(x, row, nrows) {
	if (!is.atomic(row)) { stop() }
	row <- as.integer(round(row))
	if (!(validRow(x, row))) {		stop(paste(row, 'is not a valid rownumber')) 	}
	if (!is.atomic(nrows)) {	stop() }
	nrows <- as.integer(round(nrows))
	startcell <- cellFromRowCol(x, row, 1)
	endcell <- cellFromRowCol(x, row+nrows-1, x@ncols)

	readrow <- FALSE
	if (dataContent(x) == 'nodata') {
		readrow <- TRUE
	} else if (dataContent(x) == 'all'){
		res <- x@data@values[startcell:endcell,]
	} else if (dataContent(x) == 'row') {
		if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
			res <- x@data@values
		} else {
			readrow <- TRUE
		}
	} else {
		stop('something is wrong with the RasterLayer dataContent')
	}
	if (readrow) {
		res <- .readRasterBrickValues(x, row, nrows)@data@values
	}
	colnames(res) <- layerNames(x)
	return(res)
}
)
