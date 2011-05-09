# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


sampleRegular <- function( x, size, extent=NULL, cells=FALSE, asRaster=FALSE, corners=FALSE ) {
	
	size <- round(size)
	if (size < 1) { stop('size < 1') }
	
	if (is.null(extent)) {
		if (size >= ncell(x)) {
			if (asRaster) { 
				return(x) 
			} else { 
				return(getValues(x)) 
			}
		}
		rcut <- raster(x)
		firstrow <- 1
		lastrow <- nrow(rcut)
		firstcol <- 1
		lastcol <- ncol(rcut)
		
	} else {
		extent <- alignExtent(extent, x)
		rcut <- crop(raster(x), extent)
		if (size >= ncell(rcut)) {
			x <- crop(x, extent)
			if (asRaster) { 
				return(x) 
			} else { 
				return(getValues(x)) 
			}
		}
		firstrow <- rowFromY(x, ymax(rcut))
		lastrow <- rowFromY(x, ymin(rcut)+0.5 *yres(rcut))
		firstcol <- colFromX(x, xmin(rcut))
		lastcol <- colFromX(x, xmax(rcut)-0.5 *xres(rcut))
	}
	

	X <- sqrt(ncell(rcut)/size)
	Y <- X
	nr <- max(1,floor((lastrow - firstrow + 1) / Y))
	rows <- (lastrow - firstrow + 1)/nr * 1:nr + firstrow - 1
	if (corners) {
		rows <- c(firstrow, rows, lastrow)	
	} else {
		rows <- rows - (0.5 * (lastrow - firstrow + 1)/nr)
	}
		
	nc <- max(1, floor((lastcol - firstcol + 1) / X))
	cols <- (lastcol - firstcol + 1)/nc * 1:nc  + firstcol - 1
	if (corners) {
		cols <- c(firstcol, cols, lastcol)
	} else {
		cols <- cols - (0.5 * (lastcol - firstcol + 1)/nc)
	}
	cols <- unique(round(cols))
	rows <- unique(round(rows))
	cols = cols[cols>0]
	rows = rows[rows>0]
	nr <- length(rows)
	nc <- length(cols)
	
	cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))
	
	if ( ! inMemory(x) ) { 
		if (canProcessInMemory(x, 4)) {
			x <- readAll(x)
		}
	}
	
	m <- .cellValues(x, cell)

	if (asRaster) {
		if (is.null(extent))  {
			outras <- raster(nrow=nr, ncol=nc, xmn=xmin(x), xmx=xFromCol(x, cols[length(cols)])+0.5*xres(x), ymn=yFromRow(x, rows[length(rows)])-0.5*yres(x), ymx=ymax(x), crs=projection(x)) 
		} else {
			outras <- raster(extent) 
			nrow(outras) <- nr
			ncol(outras) <- nc
		}
		if (nlayers(x) > 1) {
			outras <- brick(outras, nl=nlayers(x))
		}
		outras <- setValues(outras, m)
		layerNames(outras) <- layerNames(x)
		return(outras)
		
	} else {
	
		if (cells) {
			m <- cbind(cell, m)
			colnames(m)[2:ncol(m)] <- layerNames(x)
		} 
		return(m)
	}	
}

