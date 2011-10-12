# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2009
# Version 0.9
# Licence GPL v3



.readAllAscii <- function(x) {
	filename <- trim(filename(x))
    if (!file.exists(filename)) { stop(paste(filename, " does not exist")) }
	v <- as.numeric( scan(filename, skip=6, what='character', quiet=TRUE) )
	if (x@file@nodatavalue < 0) {
		v[v <= x@file@nodatavalue ] <- NA 			
	} else {
		v[v == x@file@nodatavalue ] <- NA 					
	}	
	return ( v ) 
}


.readRowsAscii <- function(x, startrow, nrows, startcol=1, ncols=x@ncols) {
	if (startcol > 1 | ncols < x@ncols) {
		v <- matrix(nrow=ncols, ncol=nrows)
		endcol <- startcol+ncols-1
		skiprows <- 6 + startrow - 2 
		for (i in 1:nrows) {
			v[,i] <- as.numeric ( scan(filename(x), skip=skiprows+i, nlines=nrows, what='character', quiet=TRUE) )[startcol:endcol]
		}
		v <- as.vector(v)
	} else {
		skiprows <- 6 + startrow - 1 
		v <- as.numeric ( scan(filename(x), skip=skiprows, nlines=nrows, what='character', quiet=TRUE) )
	}
	if (x@file@nodatavalue < 0) {
		v[v <= x@file@nodatavalue ] <- NA 			
	} else {
		v[v == x@file@nodatavalue ] <- NA 					
	}
	return ( v )
}


.readCellsAscii <- function(raster, cells) {
	colrow <- matrix(ncol=5, nrow=length(cells))
	colrow <- matrix(ncol=5, nrow=length(cells))
	colrow[,1] <- colFromCell(raster, cells)
	colrow[,2] <- rowFromCell(raster, cells)
	colrow[,3] <- cells
	colrow[,4] <- NA
	rows <- na.omit(unique(colrow[order(colrow[,2]), 2]))
	for (i in 1:length(rows)) {
		v <- .readRowsAscii(raster, rows[i], 1, 1, raster@ncols)
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		colrow[colrow[,2]==rows[i],4] <- v[thisrow[,1]]
	}
	return(colrow[,4]) 
}


