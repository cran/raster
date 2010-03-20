# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


sampleRegular <- function(x, size, extent=NULL, cells=FALSE, asRaster=FALSE, corners=FALSE) {
	if (class(x) == 'RasterLayer') {
		return(.sampleRegular(raster=x, n=size, extent=extent, cells=cells, asRaster=asRaster, corners=corners))
	} else {
	# ugly & inefficient hack :
		for (i in 1:nlayers(x)) {
			if (i==1) {
				x = .sampleRegular(raster(x, i), n=size, extent=extent, cells=cells, asRaster=asRaster, corners=corners)
				v <- matrix(ncol=nlayers(x), nrow=length(x))
				v[,1] = x
			}
			v[,i] = .sampleRegular(raster(x, i), n=size, extent=extent, cells=cells, asRaster=asRaster, corners=corners)
		}
		colnames(v) <- layerNames(x)
		return(v)
	}
}	


.sampleRegular <- function(raster, n, extent=NULL, cells=FALSE, asRaster=FALSE, corners=FALSE) {
	if (n<1) {stop('n < 1')}
	
	if (is.null(extent)) {
		if (n >= ncell(raster)) {
			if (dataContent(raster) != 'all') { 
				raster <- readAll(raster) }
			if (asRaster) { 
				return(raster) 
			} else { 
				return(values(raster)) 
			}
		}
		rcut <- raster(raster)
		firstrow <- 1
		lastrow <- nrow(rcut)
		firstcol <- 1
		lastcol <- ncol(rcut)
		
	} else {
		extent <- alignExtent(extent, raster)
		rcut <- crop(raster(raster), extent)
		if (n >= ncell(rcut)) {
			raster <- crop(raster, extent)
			if (asRaster) { 
				return(raster) 
			} else { 
				return(getValues(raster)) 
			}
		}
		firstrow <- rowFromY(raster, ymax(rcut))
		lastrow <- rowFromY(raster, ymin(rcut))
		firstcol <- colFromX(raster, xmin(rcut))
		lastcol <- colFromX(raster, xmax(rcut))
	}
	

	x <- sqrt(ncell(rcut)/n)
	y <- x
	nr <- max(1,floor((lastrow - firstrow + 1) / y))
	rows <- (lastrow - firstrow + 1)/nr * 1:nr + firstrow - 1
	if (corners) {
		rows <- c(firstrow, rows, lastrow)	
	} else {
		rows <- rows - (0.5 * (lastrow - firstrow + 1)/nr)
	}
	rows <- round(rows)
	
	nc <- max(1, floor((lastcol - firstcol + 1) / x))
	cols <- (lastcol - firstcol + 1)/nc * 1:nc  + firstcol - 1
	if (corners) {
		cols <- c(firstcol, cols, lastcol)
	} else {
		cols <- cols - (0.5 * (lastcol - firstcol + 1)/nc)
	}
	cols <- unique(round(cols))
	rows <- unique(round(rows))
	nr <- length(rows)
	nc <- length(cols)
	
#	m <- matrix(ncol=nr, nrow=nc)
#	for (i in 1:nr) {
#		v <- getValues(raster, rows[i])
#		m[,i] <- v[cols]
#	}	
#	m <- as.vector(m)
	cell <- cellFromRowCol(raster, rep(rows, each=nc), rep(cols, times=nr))
	
	if ( dataContent(raster) != 'all') { 
		if (canProcessInMemory(raster, 4)) {
			raster <- readAll(raster)
		}
	}
	
	m <- .readCells(raster, cell)

	if (asRaster) {
		if (is.null(extent))  {
			outras <- raster(nrow=nr, ncol=nc, xmn=xmin(raster), xmx=xFromCol(raster, cols[length(cols)])+0.5*xres(raster) , ymn=yFromRow(raster, rows[length(rows)])-0.5*yres(raster), ymx=ymax(raster)) 
		} else {
			outras <- raster(extent) 
			nrow(outras) <- nr
			ncol(outras) <- nc
		}
		outras <- setValues(outras, m)
		return(outras)
	} else {
		if (cells) {
			#cell <- cellFromRowCol(raster, rep(rows, each=nc), rep(cols, times=nr))
			cell <- cbind(cell, m)
			colnames(cell)[2] <- 'value'
			return(cell)
		} else {
			return(m)
		}
	}	
}
