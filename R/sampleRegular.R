# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


sampleRegular <- function(x, size, extent=NULL, cells=FALSE, asRaster=FALSE, corners=FALSE) {
	if (class(x) == 'RasterLayer') {
		return(.sampleRegular(x, n=size, extent=extent, cells=cells, asRaster=asRaster, corners=corners))
	} else {
	# ugly & inefficient hack :
		if (asRaster) {
			for (i in 1:nlayers(x)) {
				if (i==1) {
					xx = .sampleRegular(raster(x, i), n=size, extent=extent, cells=cells, asRaster=asRaster, corners=corners)
					v <- matrix(ncol=nlayers(x), nrow=ncell(xx))
					v[,1] = xx@data@values
				} else {
					v[,i] = .sampleRegular(raster(x, i), n=size, extent=extent, cells=cells, asRaster=asRaster, corners=corners)@data@values
				}
			}
			b = brick(xx)
			xx <- setValues(xx, v)
			layerNames(xx) <- layerNames(x)
			return(xx)
		} else {
			for (i in 1:nlayers(x)) {
				if (i==1) {
					x = .sampleRegular(raster(x, i), n=size, extent=extent, cells=cells, asRaster=asRaster, corners=corners)
					v <- matrix(ncol=nlayers(x), nrow=length(x))
					v[,1] = x
				} else {
					v[,i] = .sampleRegular(raster(x, i), n=size, extent=extent, cells=cells, asRaster=asRaster, corners=corners)
				}
			}
			colnames(v) <- layerNames(x)
			return(v)
		}
	}
}	


.sampleRegular <- function(x, n, extent=NULL, cells=FALSE, asRaster=FALSE, corners=FALSE) {

	if (n<1) {stop('n < 1')}
	
	if (is.null(extent)) {
		if (n >= ncell(x)) {
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
		if (n >= ncell(rcut)) {
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
	

	X <- sqrt(ncell(rcut)/n)
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
	
#	m <- matrix(ncol=nr, nrow=nc)
#	for (i in 1:nr) {
#		v <- getValues(raster, rows[i])
#		m[,i] <- v[cols]
#	}	
#	m <- as.vector(m)
	cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))
	
	if ( dataContent(x) != 'all') { 
		if (canProcessInMemory(x, 4)) {
			x <- readAll(x)
		}
	}
	
	m <- .readCells(x, cell)

	if (asRaster) {
		if (is.null(extent))  {
			outras <- raster(nrow=nr, ncol=nc, xmn=xmin(x), xmx=xFromCol(x, cols[length(cols)])+0.5*xres(x), ymn=yFromRow(x, rows[length(rows)])-0.5*yres(x), ymx=ymax(x), crs=projection(x)) 
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
