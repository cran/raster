# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.readRasterCellsNetCDF <- function(x, cells) {

# read all
	if (canProcessInMemory(x, 2)) {
		r <- getValues(x)
		r <- r[cells]
		return(r)
	} 
	
# read only rows needed	
	row1 <- rowFromCell(x, min(cells))
	row2 <- rowFromCell(x, max(cells))
	ncl <- (row2 - row1 + 1) * x@ncols
	r <- raster(nrow=1, ncol=ncl)

	if (canProcessInMemory(r, 2)) {
		v <- getValues(x, row1, row2-row1+1)
		v <- v[cells-cellFromRowCol(x, row1, 1)+1]
		return(v)
	}
	
# read row by row
	colrow <- matrix(ncol=3, nrow=length(cells))
	colrow[,1] <- colFromCell(x, cells)
	colrow[,2] <- rowFromCell(x, cells)
	colrow[,3] <- NA
	rows <- sort(unique(colrow[,2]))
	readrows = rows
	if ( x@file@toptobottom ) { readrows <- x@nrows - readrows + 1	}

	zvar = x@data@zvar
	time = x@data@band
	
	nc <- open.ncdf(x@file@name)
	on.exit( close.ncdf(nc) )
	
	count = c(x@ncols, 1, 1)
	for (i in 1:length(rows)) {
		start = c(1, readrows[i], time)
		values <- as.vector(get.var.ncdf(nc, varid=zvar, start=start, count=count))
		thisrow <- subset(colrow, colrow[,2] == rows[i])
		colrow[colrow[,2]==rows[i], 3] <- values[thisrow[,1]]
	}	
	
	colrow <- colrow[,3]
	#if (!is.na(x@file@nodatavalue)) { colrow[colrow==x@file@nodatavalue] <- NA	}
	#colrow <- x@data@add_offset + colrow * x@data@scale_factor

	return(colrow) 
}



.readBrickCellsNetCDF <- function(x, cells, layer, nlayers) {

		
	if (length(cells) > 1000) {
		if (canProcessInMemory(x, 2)) {
# read all
			endlayer = layer+nlayers-1
			r <- getValues(x)
			r <- r[cells, layer:endlayer]
			return(r)
		}
	} 

	
# read cell by cell
	zvar = x@data@zvar
	cols <- colFromCell(x, cells)
	rows <- rowFromCell(x, cells)
	if ( x@file@toptobottom ) { rows <- x@nrows - rows + 1 }
		
	nc <- open.ncdf(x@file@name)
	on.exit( close.ncdf(nc) )
	
	count = c(1, 1, nlayers)
	res <- matrix(nrow=length(cells), ncol=nlayers)
	for (i in 1:length(cells)) {
		start = c(cols[i], rows[i], layer)
		res[i,] <- get.var.ncdf(nc, varid=zvar, start=start, count=count)
	}	

	#if (!is.na(x@file@nodatavalue)) { res[res==x@file@nodatavalue] <- NA	}
	#res <- x@data@add_offset + res * x@data@scale_factor

	return(res) 
}

