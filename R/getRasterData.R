# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

.getRasterData <- function(raster, rownr, startcol, ncolumns, dtype, dsize, dsign, offset=0) {
	raster <- openConnection(raster)
	offset <- offset + raster@file@offset
	if (rownr > 0) {
		if (! raster@file@toptobottom ) {
			rownr <- nrow(raster) - rownr + 1
		}
		seek(raster@file@con, ((rownr-1) * ncol(raster) + (startcol-1) + offset) * dsize)
		result <- readBin(raster@file@con, what=dtype, n=ncolumns, dsize, dsign, endian=raster@file@byteorder) 
	} else {	
		seek(raster@file@con, offset)
		result <- readBin(raster@file@con, what=dtype, n=ncell(raster), dsize, dsign, endian=raster@file@byteorder)
		if (! raster@file@toptobottom ) {
			result <- matrix(result, ncol=ncol(raster), byrow=TRUE)
			result <- as.vector( t (result[nrow(raster):1,] ) )
		}
	}
	raster <- closeConnection(raster)
	return(result)
}


.getBilRasterData <- function(raster, rownr, startcol, ncolumns, dtype, dsize, dsign, band) {
	raster <- openConnection(raster)
	if (rownr > 0) {
		seek(raster@file@con, ((rownr-1) * ncol(raster) * nbands(raster) + (startcol-1) + (band-1) * ncol(raster)) * dsize)
		result <- readBin(raster@file@con, what=dtype, n=ncolumns, dsize, dsign, endian=raster@file@byteorder)
	} else {	
		result <- vector(length=0)
		rowoff <- ncol(raster) * nbands(raster)
		for (rownr in 1:nrow(raster)) {
			seek(raster@file@con, ((rownr-1) * rowoff + startcol) * dsize)
			res <- readBin(raster@file@con, what=dtype, n=ncolumns, dsize, dsign, endian=raster@file@byteorder)
			result <- c(result, res)
		}
	}
	raster <- closeConnection(raster)
	return(result)
}


.getBipRasterData <- function(raster, rownr, startcol, ncolumns, dtype, dsize, dsign, band) {
	index <- rep(FALSE, nbands(raster))
	index[band] <- TRUE
	index <- rep(index, ncolumns)
	raster <- openConnection(raster)
	if (rownr > 0) {
		seek(raster@file@con, ((rownr-1) * ncol(raster) * nbands(raster) + (startcol-1) * nbands(raster)) * dsize)
		nc <- ncolumns * nbands(raster)
		result <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
		result <- result[index]
	} else {	
		result <- vector(length=0)
		nc <- ncolumns * nbands(raster)
		rowoff <- ncol(raster) * nbands(raster)
		startcol <- (startcol-1) * nbands(raster) + (startcol-1)
		for (rownr in 1:nrow(raster)) {
			seek(raster@file@con, ((rownr-1) * rowoff + startcol) * dsize)
			res <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
			res <- res[index]
			result <- c(result, res)
		}
	}
	raster <- closeConnection(raster)
	return(result)
}

