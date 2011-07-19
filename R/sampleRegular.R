# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


sampleRegular <- function( x, size, ext=NULL, cells=FALSE, asRaster=FALSE) {
	
	size <- round(size)
	stopifnot(size > 0)
	nl <- nlayers(x)
	rotated <- rotated(x)
	
	if (is.null(ext)) {
		if (size >= ncell(x)) {
			if (asRaster) { 
				if (!rotated) {
					return(x) 
				}
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
	
		rcut <- crop(raster(x), ext)
		ext <- extent(rcut)
		if (size >= ncell(rcut)) {
			x <- crop(x, ext)
			if (asRaster) { 
				return(x) 
			} else { 
				return(getValues(x)) 
			}
		}
		yr <- yres(rcut)
		xr <- xres(rcut)
		firstrow <- rowFromY(x, ext@ymax-0.5 *yr)
		lastrow <- rowFromY(x, ext@ymin+0.5*yr)
		firstcol <- colFromX(x, ext@xmin+0.5*xr)
		lastcol <- colFromX(x, ext@xmax-0.5*xr)
	}
	

	Y <- X <- sqrt(ncell(rcut)/size)
	nr <- max(1, floor((lastrow - firstrow + 1) / Y))
	nc <- max(1, floor((lastcol - firstcol + 1) / X))

	rows <- (lastrow - firstrow + 1)/nr * 1:nr + firstrow - 1
	rows <- rows - (0.5 * (lastrow - firstrow + 1)/nr)
	cols <- (lastcol - firstcol + 1)/nc * 1:nc  + firstcol - 1
	cols <- cols - (0.5 * (lastcol - firstcol + 1)/nc)

	cols <- unique(round(cols))
	rows <- unique(round(rows))
	cols = cols[cols>0]
	rows = rows[rows>0]
	nr <- length(rows)
	nc <- length(cols)
	

	if (fromDisk(x)) {
		driver <- .driver(x, FALSE)
		if (driver=='gdal' & !rotated & !cells) {
	
			offs <- c(firstrow,firstcol)-1
			reg <- c(nrow(rcut), ncol(rcut))-1
			if (nl == 1) {
				band <- bandnr(x)
			} else {
				band <- NULL
			}
			con <- GDAL.open(x@file@name, silent=TRUE)
			v <- getRasterData(con, band=band, offset=offs, region.dim=reg, output.dim=c(nr, nc)) 
			closeDataset(con)
			if (x@data@gain != 1 | x@data@offset != 0) {
				v <- v * x@data@gain + x@data@offset
			}
			if (x@file@nodatavalue < 0) {
				v[v <= x@file@nodatavalue] <- NA
			} else {
				v[v == x@file@nodatavalue] <- NA
			}
	
			if (asRaster) {
				if (is.null(ext))  {
					outras <- raster(x)
				} else {
					outras <- raster(ext) 
				}
				nrow(outras) <- nr
				ncol(outras) <- nc
				if (nl > 1) {
					outras <- brick(outras, nl=nl)
					return( setValues(outras, v))
				} else {
					return( setValues(outras, as.vector(v)))
				}
			} else {
				return(v)
			}
		}
	}
	
	cell <- cellFromRowCol(x, rep(rows, each=nc), rep(cols, times=nr))
	
	if ( ! inMemory(x) ) { 
		if (canProcessInMemory(x, 5)) {
			x <- readAll(x)
		}
	}
	
	if (asRaster) {
		if (rotated) {
			if (is.null(ext)) {
				outras <- raster(extent(x))
			} else {
				outras <- raster(ext)
			}
			ncol(outras) <- nc
			nrow(outras) <- nr
			xy <- xyFromCell(outras, 1:ncell(outras))
			m <- .xyValues(x, xy)
			
		} else {
			m <- .cellValues(x, cell)

			if (is.null(ext))  {
				outras <- raster(x)
			} else {
				outras <- raster(ext) 
			}
			nrow(outras) <- nr
			ncol(outras) <- nc
			
		}
		if (nl > 1) {
			outras <- brick(outras, nl=nl)
		}
		outras <- setValues(outras, m)
		layerNames(outras) <- layerNames(x)
		return(outras)
		
	} else {
	
		m <- .cellValues(x, cell)
		if (cells) {
			m <- cbind(cell, m)
			colnames(m)[2:ncol(m)] <- layerNames(x)
		} 
		return(m)
	}	
}

