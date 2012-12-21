# Author: Robert J. Hijmans
# Date :  March  2009, August 2012
# Licence GPL v3
# updated November 2011
# version 1.0



fourCellsFromXY <- function(object, xy, duplicates=TRUE) {
# if duplicates is TRUE, the same cell number can be returned 
# twice (if point in the middle of division between two cells) or
# four times (if point in center of cell)
	r <- raster(object) # use small object
	stopifnot(is.matrix(xy))
	cells <- cellFromXY(r, xy)
	rows <- rowFromCell(r, cells)
	cols <- colFromCell(r, cells)
	cellsXY <- xyFromCell(r, cells)

	if (duplicates) {
		pos <- matrix(0, ncol=ncol(xy), nrow=nrow(xy))
		pos[ xy[,1] > cellsXY[,1], 1 ] <- 1
		pos[ xy[,1] < cellsXY[,1], 1 ] <- -1
		pos[ xy[,2] < cellsXY[,2], 2 ] <- 1
		pos[ xy[,2] > cellsXY[,2], 2 ] <- -1
	} else {
		pos <- matrix(-1, ncol=ncol(xy), nrow=nrow(xy))
		pos[ xy[,1] > cellsXY[,1], 1 ] <- 1
		pos[ xy[,2] < cellsXY[,2], 2 ] <- 1
	}
	
	
	poscol <- cols + pos[,1]
	if (raster:::.isGlobalLonLat(r)) {
		poscol[poscol==0] <- ncol(r)
		poscol[poscol==ncol(r)+1] <- 1
	} else {
		poscol[poscol==0] <- 2
		poscol[poscol==ncol(r)+1] <- ncol(r) - 1
	}
	
	posrow <- rows + pos[,2]
	posrow[posrow==0] <- 2
	posrow[posrow==nrow(r)+1] <- nrow(r) - 1

	four <- matrix(cells, ncol=4, nrow=nrow(xy))
	four[,2] <- cellFromRowCol(r, posrow, cols)
	four[,3] <- cellFromRowCol(r, posrow, poscol)
	four[,4] <- cellFromRowCol(r, rows, poscol)
	
	return(four)
}

