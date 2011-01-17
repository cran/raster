# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  March  2009
# Version 0.9
# Licence GPL v3


.bilinearValue <- function(raster, xyCoords, na.rm=FALSE, layer, n) {

	fourCellsFromXY <- function(raster, xy) {
		cells <- cellFromXY(raster, xy)
		row <- rowFromCell(raster, cells)
		col <- colFromCell(raster, cells)
		cellsXY <- xyFromCell(raster, cells)

		pos <- matrix(-1, ncol=ncol(xy), nrow=nrow(xy))
		pos[ xy[,1] > cellsXY[,1], 1 ] <- 1
		pos[ xy[,2] < cellsXY[,2], 2 ] <- 1

		poscol <- col + pos[,1]
		poscol[poscol==0] <- 2
		poscol[poscol==ncol(raster)+1] <- ncol(raster) - 1
		posrow <- row + pos[,2]
		posrow[posrow==0] <- 2
		posrow[posrow==nrow(raster)+1] <- nrow(raster) - 1

		four <- matrix(ncol=4, nrow=nrow(xy))
		four[,1] <- cells
		four[,2] <- cellFromRowCol(raster, posrow, col)
		four[,3] <- cellFromRowCol(raster, posrow, poscol)
		four[,4] <- cellFromRowCol(raster, row, poscol)
		return(four)
	}
	
	bilinear <- function(x,y, x1,x2,y1,y2, v) {
		v = v / ((x2-x1)*(y2-y1))
		return( v[,1]*(x2-x)*(y2-y) + v[,3]*(x-x1)*(y2-y) + v[,2]*(x2-x)*(y-y1) + v[,4]*(x-x1)*(y-y1) )
		#div <- (x2-x1)*(y2-y1)
		#return ( (v[,1]/div)*(x2-x)*(y2-y) + (v[,3]/div)*(x-x1)*(y2-y) + (v[,2]/div)*(x2-x)*(y-y1) + (v[,4]/div)*(x-x1)*(y-y1) )
	}
	
	four <- fourCellsFromXY(raster, xyCoords)
	xy4 <- matrix(xyFromCell(raster, as.vector(four)), ncol=8)
	x <- apply(xy4[,1:4,drop=FALSE], 1, range)
	y <- apply(xy4[,5:8,drop=FALSE], 1, range)
	xy4 <- cbind(c(x[1,], x[1,], x[2,], x[2,]), c(y[1,], y[2,], y[1,], y[2,]))
	cells <- cellFromXY(raster, xy4)
	
	nls <- nlayers(raster)
	if (nls == 1) {
		v <- matrix( .cellValues(raster, cells), ncol=4)
		bilinear(xyCoords[,1], xyCoords[,2], x[1,], x[2,], y[1,], y[2,], v)
	} else {
	
		if (missing(layer)) { layer <- 1 }
		if (missing(n)) { n <- (nls-layer+1) }
		lyrs <- layer:(layer+n-1)
		res <- matrix(ncol=length(lyrs), nrow=nrow(xyCoords))
		cv <- .cellValues(raster, cells, layer=layer, nl=n)
		for (i in 1:ncol(cv)) {
			v <- matrix(cv[, i], ncol=4)
			res[,i] <- bilinear(xyCoords[,1], xyCoords[,2], x[1,], x[2,], y[1,], y[2,], v)
		}
		colnames(res) <- layerNames(raster)[lyrs]
		return(res)
	}
}

