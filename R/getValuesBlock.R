# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("getValuesBlock")) {
	setGeneric("getValuesBlock", function(x, row, ...)
		standardGeneric("getValuesBlock"))
}	



setMethod('getValuesBlock', signature(x='RasterStack', row='numeric'), 
	function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1)) {
		for (i in 1:nlayers(x)) {
			if (i==1) {
				v <- getValuesBlock(x@layers[[i]], row, nrows, col, ncols)
				res <- matrix(ncol=nlayers(x), nrow=length(v))
				colnames(res) <- layerNames(x)
				res[,1] <- v
			} else {
				res[,i] <- getValuesBlock(x@layers[[i]], row, nrows, col, ncols)
			}
		}
		res
	}
)



setMethod('getValuesBlock', signature(x='RasterBrick', row='numeric'), 
	function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1)) {
	
		if (dataContent(x) == 'all'){
			row <- as.integer(round(row))
			nrows <- as.integer(round(nrows))
			lastrow <- row + nrows - 1
			col <- as.integer(round(col))
			ncols <- as.integer(round(ncols))
			lastcol <- col + ncols - 1
			if (col==1 & ncols==ncol(x)) {
				if (row==1 & nrows==nrow(x)) {
					res <- x@data@values
				} else {
					start = cellFromRowCol(x, row, 1)
					end =  cellFromRowCol(x, lastrow, ncol(x))
					res <- x@data@values[start:end, ]
				}
			} else {
				cells <- cellFromRowColCombine(x, row:lastrow, col:lastcol)
				res <- x@data@values[cells, ]
			}
		} else {
			for (i in 1:nlayers(x)) {
				# to do: need a more efficient function here that only goes to disk once.
				if (i==1) {
					v <- values(.readRasterLayerValues(raster(x, i), row, nrows, col, ncols))
					res <- matrix(ncol=nlayers(x), nrow=length(v))
					res[,1] <- v
				} else {
					res[,i] <- values(.readRasterLayerValues(raster(x, i), row, nrows, col, ncols))
				}
			}
		}
		colnames(res) = layerNames(x)
		res
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
		
		readrow <- FALSE
		startcell <- cellFromRowCol(x, row, col)
		lastcell <- cellFromRowCol(x, lastrow, lastcol)

		if (!(validRow(x, row))) {	stop(paste(row, 'is not a valid rownumber')) }
	
		if (dataContent(x) == 'nodata') {
			
			if (dataSource(x) == 'ram') {
				return(rep(NA, times=(lastcell-startcell+1)))
			}
			
			readrow <- TRUE			
			
		} else if (dataContent(x) == 'all') {
		
			if (col==1 & ncols==ncol(x)) {
				res <- x@data@values[startcell:lastcell]
			} else {
				cells <- cellFromRowColCombine(x, row:lastrow, col:lastcol)
				res <- x@data@values[cells]
			}

		} else if (dataContent(x) == 'rows') {
		
			if ( (dataIndices(x)[1] <= startcell) & (dataIndices(x)[2] >= endcell) ) {
				cells <- cellFromRowColCombine(x, row:lastrow, col:lastcol) - dataIndices(x)[1] + 1
				res <- x@data@values[cells]
			} else {
				readrow <- TRUE
			}
			
		} else if (dataContent(x) == 'row') {
		
			if ( (dataIndices(x)[1] == startcell) & (dataIndices(x)[2] == endcell) ) {
				res <- x@data@values[col:lastcol]
			} else {
				readrow <- TRUE
			}
			
		} else if (dataContent(x) == 'block') {
		
			fcol <- colFromCell(x, dataIndices(x)[1])
			lcol <- colFromCell(x, dataIndices(x)[2])
			if (fcol > col | lastcol < lastcol) {
				readrow <- TRUE
			} else {
				frow <- rowFromCell(x, dataIndices(x)[1])
				lrow <- rowFromCell(x, dataIndices(x)[2])
				if (frow > row | lrow < lastrow) {
					readrow <- TRUE
				} else {
					cells <- cellFromRowColCombine(x, row:lastrow, col:lastcol)
					rown <- rowFromCell(x, cells) - frow + 1
					coln <- colFromCell(x, cells) - fcol + 1
					res <- as.vector(matrix(x@data@values, nrow=lrow-frow+1, byrow=TRUE)[cbind(rown, coln)])
				}
			}
		} else {
			stop('something is wrong with the RasterLayer dataContent')
		}
	
		if (readrow) {	
			res <- values(.readRasterLayerValues(x, row, nrows, col, ncols))
		}
		if (format=='matrix') {
			res = matrix(res, nrow=nrows , ncol=ncols )
			colnames(res) <- col:lastcol
			rownames(res) <- row:lastrow
		}
		res
	}
	
)

