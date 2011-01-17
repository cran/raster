# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

#read data on the raster for cell numbers


.readCells <- function(x, cells) {
	cells <- cbind(1:length(cells), cells)
	cells <- cells[order(cells[,2]), ,drop=FALSE]
	uniquecells <- sort(na.omit(unique(cells[,2])))
	uniquecells <- uniquecells[(uniquecells > 0) & (uniquecells <= ncell(x))]

	if (length(uniquecells) > 0) {
		if ( inMemory(x) ) {
			vals <- getValues(x)[uniquecells]
		} else if ( fromDisk(x) ) {
			if (length(uniquecells) > 100 & canProcessInMemory(x, 2)) {
				vals <- getValues(x)
				vals <- vals[uniquecells]
			} else if (x@file@driver == 'gdal') {
				vals <- .readCellsGDAL(x, uniquecells)
			} else if (x@file@driver == 'ascii') {
				vals <- .readCellsAscii(x, uniquecells)			
			} else if (x@file@driver == 'netcdf') {
				vals <- .readRasterCellsNetCDF(x, uniquecells)			
			} else {
				vals <- .readCellsRaster(x, uniquecells)
			}	
		} else { 
			stop('no data on disk or in memory')
		}	
	} else {
		return(rep(NA, times=length(cells[,1])))
	}
	
	vals <- cbind(uniquecells, vals)
	vals <- merge(x=cells[,2], y=vals, by=1, all=TRUE)
	vals <- cbind(cells[,1], vals[,2])
#	vals <- vals[order(cells[,1]), 2, drop=FALSE]
	vals <- vals[order(cells[,1]), 2]
	
	if (x@data@gain != 1 | x@data@offset != 0) {
		vals <- vals * x@data@gain + x@data@offset
	}
	
	return(vals)
}


.readCellsGDAL <- function(x, cells) {

	if (! .requireRgdal() ) { stop('rgdal not available') }

	colrow <- matrix(ncol=3, nrow=length(cells))
	colrow[,1] <- colFromCell(x, cells)
	colrow[,2] <- rowFromCell(x, cells)
	colrow[,3] <- NA
	rows <- sort(unique(colrow[,2]))

	nc <- x@ncols
	con <- GDAL.open(x@file@name, silent=TRUE)
	for (i in 1:length(rows)) {
		offs <- c(rows[i]-1, 0) 
		v <- getRasterData(con, offset=offs, region.dim=c(1, nc), band = x@data@band)
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		colrow[colrow[,2]==rows[i], 3] <- v[thisrow[,1]]
	}
	closeDataset(con)

	# if  NAvalue() has been used.....
	if (x@file@nodatavalue < 0) {
		colrow[colrow[, 3] <= x@file@nodatavalue, 3] <- NA 			
	} else {
		colrow[colrow[, 3] == x@file@nodatavalue, 3] <- NA 					
	}		
	return(colrow[, 3]) 
}	


.readCellsRaster <- function(x, cells) {
	res <- vector(length=length(cells))
	res[] <- NA
	dsize <- dataSize(x@file@datanotation)
	if (.shortDataType(x@file@datanotation) == "FLT") { 
		dtype <- "numeric"
	} else { 
		dtype <- "integer"
	}
	signed <- dataSigned(x@file@datanotation)
	
	if (! x@file@toptobottom) {
		rows <- rowFromCell(x, cells)
		cols <- colFromCell(x, cells)
		rows <- nrow(x) - rows + 1
		cells <- cellFromRowCol(x, rows, cols)
	}
	cells <- cells + x@file@offset
	
	if (nbands(x) > 1) {
		if (.bandOrder(x) == 'BIL') {
			cells <- cells + (rowFromCell(x, cells)-1) * x@ncols * (nbands(x)-1) + (band(x)-1) * x@ncols
		} else if (.bandOrder(x) == 'BIP') {
			cells <- (cells - 1) * nbands(x) + band(x) - 1
		} else if (.bandOrder(x) == 'BSQ') {	
			cells <- cells + (band(x)-1) * ncell(x)
		}
	}
	
	x <- openConnection(x)
	for (i in seq(along=cells)) {
		seek(x@file@con, (cells[i]-1) * dsize)
		res[i] <- readBin(x@file@con, what=dtype, n=1, size=dsize, endian=x@file@byteorder, signed=signed) 
	}
	x <- closeConnection(x)
	
	if (x@file@datanotation == 'INT4U') {
		res[res < 0] <- 2147483647 - res[res < 0] 
	}
	
	if (dtype == "numeric") {
		res[is.nan(res)] <- NA
		res[res <= x@file@nodatavalue] <- NA
	} else {
		res[res == x@file@nodatavalue] <- NA
	}
	
	return(res)
}

