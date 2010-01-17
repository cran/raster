# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009
# Version 0.9
# Licence GPL v3


.readRasterLayerValues <- function(object, startrow, nrows=1, startcol=1, ncols=ncol(object)-startcol+1) {

	if (nrows < 1) { stop("nrows should be > 1") }
	startrow <- min(max(1, round(startrow)), object@nrows)
	endrow <- min(object@nrows, startrow+nrows-1)
	nrows <- endrow - startrow + 1

	if (ncols < 1) { stop("ncols should be > 1") }
	startcol <- min(max(1, round(startcol)), object@ncols)
	endcol <- min(object@ncols, startcol+ncols-1)
	ncols <- endcol - startcol + 1
		
	if (.isNativeDriver(object@file@driver))  {

		getBSQData <- function(raster, r, nrows, c, ncols, dtype, dsize, dsign, band=1) {
			offset <- raster@file@offset + (band-1) * raster@ncols * raster@nrows + (r-1) * raster@ncols 
			if (c==1 & ncols==raster@ncols) {
				seek(raster@file@con, offset * dsize)
				result <- readBin(raster@file@con, what=dtype, n=nrows*ncols, dsize, dsign, endian=raster@file@byteorder) 
			} else {
				result <- matrix(ncol=nrows, nrow=ncols)
				for (i in 1:nrows) {
					off <- offset + (i-1) * raster@ncols + (c-1)
					seek(raster@file@con, off * dsize)
					result[,i] <- readBin(raster@file@con, what=dtype, n=ncols, dsize, dsign, endian=raster@file@byteorder) 
				}
			}
			return(as.vector(result))
		}
		
		getBilData <- function(raster, r, nrows, c, ncols, dtype, dsize, dsign, band) {
			offset <- raster@file@offset + raster@file@nbands * raster@ncols * (r-1) + (c-1)
			result <- matrix(ncol=nrows, nrow=ncols)
			for (i in 1:nrows) {
				off <- offset + (i-1) * raster@ncols * raster@file@nbands + (band-1) * raster@ncols 
				seek(raster@file@con, off * dsize)
				result[,i] <- readBin(raster@file@con, what=dtype, n=ncols, dsize, dsign, endian=raster@file@byteorder)
			}	
			return(as.vector(result))
		}

		getBipData <- function(raster, r, nrows, c, ncols, dtype, dsize, dsign, band) {
			offset <- raster@file@offset + raster@file@nbands * raster@ncols * (r-1) 
			nc <- ncols * raster@file@nbands
			index <- rep(FALSE, raster@file@nbands)
			index[band] <- TRUE
			index <- rep(index, ncols)
			result <- matrix(ncol=nrows, nrow=ncols)
			for (i in 1:nrows) {
				off <- offset + (i-1) * raster@ncols * raster@file@nbands + (c-1) * raster@file@nbands 
				seek(raster@file@con, off * dsize)
				res <- readBin(raster@file@con, what=dtype, n=nc, dsize, dsign, endian=raster@file@byteorder) 
				result[,i] <- res[index]
			}
			return(as.vector(result))
		}
		

		if (! object@file@toptobottom ) {
			endrow <- object@nrows - startrow + 1
			startrow <- endrow - nrows + 1
		}
		dtype <- substr(object@file@datanotation, 1, 3)
		if (dtype == "INT" | dtype == "LOG" ) { 
			dtype <- "integer"
		} else {
			dtype <- "numeric" 
		}
		dsize <- dataSize(object@file@datanotation)
		dsign <- dataSigned(object@file@datanotation)
		
		object <- openConnection(object)
		if (object@file@nbands > 1) {
			band <- object@data@band
			bo <- object@file@bandorder
			if (bo == 'BSQ') {
				result <- getBSQData(object, r=startrow, nrows=nrows, c=startcol, ncols=ncols, dtype=dtype, dsize=dsize, dsign=dsign, band=band) 
			} else if (bo == 'BIL') {
				result <- getBilData(object, r=startrow, nrows=nrows, c=startcol, ncols=ncols, dtype=dtype, dsize=dsize, dsign=dsign, band=band) 
			} else if (bo == 'BIP') {
				result <- getBipData(object, r=startrow, nrows=nrows, c=startcol, ncols=ncols, dtype=dtype, dsize=dsize, dsign=dsign, band=band) 
			} 
		} else {
			result <- getBSQData(object, r=startrow, nrows=nrows, c=startcol, ncols=ncols, dtype=dtype, dsize=dsize, dsign=dsign) 
		}
		object <- closeConnection(object)

		if (! object@file@toptobottom ) {
			result <- t(matrix(result, nrow=ncols, ncol=nrows))
			result <- result[nrows:1,]
			result <- as.vector(t(result))
		}

		if (object@file@datanotation == 'INT4U') {
			result[result < 0] <- 2147483647 - result[result < 0] 
		}
		if (dtype == 'numeric') {
			result[result <= (0.999999 * object@file@nodatavalue)] <- NA 	
			result[is.nan(result)] <- NA
		} else {
			result[result == object@file@nodatavalue ] <- NA 			
		}
		if (dtype == 'logical') {
			result <- as.logical(result)
		}
		

# ascii is internal to this package but not 'native' (not binary)
	} else if (object@file@driver == 'ascii') {
		result <- .readRowsAscii(object, startrow, nrows, startcol, ncols)
		
	} else if (object@file@driver == 'netcdf') {
		result <- .readRowsNetCDF(object, startrow, nrows, startcol, ncols)
		
		
#use GDAL  		
	} else { 
		offs <- c((startrow-1), (startcol-1)) 
		reg <- c(nrows, ncols)
		con <- GDAL.open(object@file@name, silent=TRUE)
		result <- getRasterData(con, offset=offs, region.dim=reg, band=object@data@band)
		closeDataset(con)
		result <- as.vector(result)
		
		# if  NAvalue() has been used.....
		if (object@file@nodatavalue < 0) {
			result[result <= object@file@nodatavalue ] <- NA 			
		} else {
			result[result == object@file@nodatavalue ] <- NA 					
		}
		
	} 
	
	if (object@data@gain != 1 | object@data@offset != 0) {
		result <- result * object@data@gain + object@data@offset
	}

	return(result)
}

