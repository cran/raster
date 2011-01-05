# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("getValuesBlock")) {
	setGeneric("getValuesBlock", function(x, row, ...)
		standardGeneric("getValuesBlock"))
}	



setMethod('getValuesBlock', signature(x='RasterStack', row='numeric'), 
	function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), lyrs) {
		nl <- nlayers(x)
		if (missing(lyrs)) {
			lyrs <- 1:nl
		} else {
			lyrs <- lyrs[lyrs %in% 1:nl]
			if (length(lyrs) == 0) {
				stop("no valid layers selected")
			}
		}
		nlyrs <- length(lyrs)
		nrows <- min(round(nrows), x@nrows-row+1)
		ncols <- min((x@ncols-col+1), ncols)
		stopifnot(nrows > 0)
		stopifnot(ncols > 0)
		
		res <- matrix(ncol=nlyrs, nrow=nrows * ncols)
		for (i in 1:nlyrs) {
			res[,i] <- getValuesBlock(x@layers[[lyrs[i]]], row, nrows, col, ncols)
		}
		colnames(res) <- layerNames(x)[lyrs]
		res
	}
)



setMethod('getValuesBlock', signature(x='RasterBrick', row='numeric'), 
	function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), lyrs) {

		nrows <- min(round(nrows), x@nrows-row+1)
		ncols <- min((x@ncols-col+1), ncols)
		stopifnot(nrows > 0)
		stopifnot(ncols > 0)

		
		nlyrs <- nlayers(x)
		if (missing(lyrs)) {
			lyrs <- 1:nlyrs
		} else {
			lyrs <- lyrs[lyrs %in% 1:nlyrs]
			if (length(lyrs) == 0) {
				stop("no valid layers")
			}
			nlyrs <- length(lyrs)
		}
		
		
		if ( inMemory(x) ){
			row <- max(1, as.integer(round(row)))
			nrows <- round(nrows)
			nrows <- min(nrows, x@nrows-row+1)
			lastrow <- row + nrows - 1
			col <- as.integer(round(col))
			ncols <- as.integer(round(ncols))
			lastcol <- col + ncols - 1
			if (col==1 & ncols==ncol(x)) {
				if (row==1 & nrows==nrow(x)) {
					res <- x@data@values[,lyrs]
				} else {
					start = cellFromRowCol(x, row, 1)
					end =  cellFromRowCol(x, lastrow, ncol(x))
					res <- x@data@values[start:end, lyrs]
				}
			} else {
				cells <- cellFromRowColCombine(x, row:lastrow, col:lastcol)
				res <- x@data@values[cells, lyrs]
			}
			
		} else if ( fromDisk(x) ) {
			if (x@file@driver == 'netcdf') {
				return( .readRowsBrickNetCDF(x, row, nrows, col, ncols, lyrs=lyrs) )
			} else {
				res <- matrix(ncol=nlyrs, nrow=nrows*ncols)
				for (i in 1:nlyrs) {
				# to do: need something more efficient 
					res[,i] <- .readRasterLayerValues(raster(x, lyrs[i]), row, nrows, col, ncols)
				}
			}
			
		} else {
			res <- ( matrix(rep(NA, nrows * ncols * nlyrs), ncol=nlyrs) )
		}
		
	colnames(res) <- layerNames(x)[lyrs]
	return(res)
	}
)



setMethod('getValuesBlock', signature(x='RasterLayer', row='numeric'), 
 	function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), format='') {
		
		row <- max(1, min(x@nrows, round(row[1])))
		lastrow <- min(x@nrows, row + round(nrows[1]) - 1)
		nrows <- lastrow - row + 1
		col <- max(1, min(x@ncols, round(col[1])))
		lastcol <- col + round(ncols[1]) - 1
		ncols <- lastcol - col + 1
		
		startcell <- cellFromRowCol(x, row, col)
		lastcell <- cellFromRowCol(x, lastrow, lastcol)

		if (!(validRow(x, row))) {	stop(paste(row, 'is not a valid rownumber')) }
	
		if (!  inMemory(x) ) {
			
			if (! fromDisk(x)) {
				return(rep(NA, times=(lastcell-startcell+1)))
			}
			
			res <- .readRasterLayerValues(x, row, nrows, col, ncols)
			
		} else  {
		
			if (col==1 & ncols==ncol(x)) {
				res <- x@data@values[startcell:lastcell]
			} else {
				cells <- cellFromRowColCombine(x, row:lastrow, col:lastcol)
				res <- x@data@values[cells]
			}
			
		} 
	
		if (format=='matrix') {
			res = matrix(res, nrow=nrows , ncol=ncols, byrow=TRUE )
			colnames(res) <- col:lastcol
			rownames(res) <- row:lastrow
		}
		res
	}
	
)

