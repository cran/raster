# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3



#read data on the raster for cell numbers

.brickReadCells <- function(object, cells) {
	result <- matrix(nrow=length(cells), ncol=nlayers(object))
	for (i in 1:nlayers(object)) {
		r <- raster(object, i)
		result[,i] <- .readCells(r, cells)
	}
	if (!(is.null(dim(result)))) {
		colnames(result) <- layerNames(object)
	}	
	return(result)
}



.readCells <- function(raster, cells) {
	cells <- cbind(1:length(cells), cells)
	cells <- cells[order(cells[,2]), ,drop=FALSE]
	uniquecells <- sort(na.omit(unique(cells[,2])))
	uniquecells <- uniquecells[(uniquecells > 0) & (uniquecells <= ncell(raster))]

	if (length(uniquecells) > 0) {
		if (dataContent(raster) == 'all') {
			vals <- values(raster)[uniquecells]
		} else if (dataSource(raster) == 'disk') {
			if (length(uniquecells) > 100 & canProcessInMemory(raster, 2)) {
				vals <- getValues(raster)
				vals <- vals[uniquecells]
			} else if (.driver(raster) == 'gdal') {
				vals <- .readCellsGDAL(raster, uniquecells)
			} else if (.driver(raster) == 'ascii') {
				vals <- .readCellsAscii(raster, uniquecells)			
			} else {
				vals <- .readCellsRaster(raster, uniquecells)
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
	vals <- vals[order(cells[,1]), ,drop=FALSE]
	return(vals[,2])
}


.readCellsGDAL <- function(raster, cells) {

	if (! .requireRgdal() ) { stop('rgdal not available') }

	colrow <- matrix(ncol=3, nrow=length(cells))
	colrow[,1] <- colFromCell(raster, cells)
	colrow[,2] <- rowFromCell(raster, cells)
	colrow[,3] <- NA
	rows <- sort(unique(colrow[,2]))

	nc <- ncol(raster)
	con <- GDAL.open(raster@file@name, silent=TRUE)
	for (i in 1:length(rows)) {
		offs <- c(rows[i]-1, 0) 
		values <- getRasterData(con, offset=offs, region.dim=c(1, nc), band = raster@data@band)
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		colrow[colrow[,2]==rows[i], 3] <- values[thisrow[,1]]
	}
	closeDataset(con)

	# if  NAvalue() has been used.....
	if (raster@file@nodatavalue < 0) {
		colrow[colrow[, 3] <= raster@file@nodatavalue, 3] <- NA 			
	} else {
		colrow[colrow[, 3] == raster@file@nodatavalue, 3] <- NA 					
	}		
	return(colrow[, 3]) 
}	


.readCellsRaster <- function(raster, cells) {
	res <- vector(length=length(cells))
	res[] <- NA
	dsize <- dataSize(raster@file@datanotation)
	if (.shortDataType(raster@file@datanotation) == "FLT") { 
		dtype <- "numeric"
	} else { 
		dtype <- "integer"
	}
	signed <- dataSigned(raster@file@datanotation)
	
	if (! raster@file@toptobottom) {
		rows <- rowFromCell(raster, cells)
		cols <- colFromCell(raster, cells)
		rows <- nrow(raster) - rows + 1
		cells <- cellFromRowCol(raster, rows, cols)
	}
	cells <- cells + raster@file@offset
	
	if (nbands(raster) > 1) {
		if (.bandOrder(raster) == 'BIL') {
			cells <- cells + (rowFromCell(raster, cells)-1) * ncol(raster) * (nbands(raster)-1) + (band(raster)-1) * ncol(raster)
		} else if (.bandOrder(raster) == 'BIP') {
			cells <- cells + (cells - 1) * (nbands(raster)-1) + (band(raster) - 1)
		} else if (.bandOrder(raster) == 'BSQ') {	
			cells <- cells + (band(raster)-1) * ncell(raster)
		}
	}
	
	raster <- openConnection(raster)
	for (i in seq(along=cells)) {
		seek(raster@file@con, (cells[i]-1) * dsize)
		res[i] <- readBin(raster@file@con, what=dtype, n=1, size=dsize, endian=raster@file@byteorder, signed=signed) 
	}
	raster <- closeConnection(raster)
	
	if (dtype == "numeric") {
		res[is.nan(res)] <- NA
		res[res <= raster@file@nodatavalue] <- NA
	} else {
		res[res == raster@file@nodatavalue] <- NA
	}
	return(res)
}

