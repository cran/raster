# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("sampleRandom")) {
	setGeneric("sampleRandom", function(x, size, ...)
		standardGeneric("sampleRandom"))
}	


setMethod('sampleRandom', signature(x='Raster'), 
function(x, size, na.rm=TRUE, ext=NULL, cells=FALSE, rowcol=FALSE, xy=FALSE, sp=FALSE, asRaster=FALSE, ...) {

	if (!hasValues(x)) {
		stop('No values associated with the Raster object')
	}	


	if (asRaster) {
		if (! is.null(ext)) {
			x <- crop(x, ext)
		}
		if (size >= ncell(x)) {
			return(x)
		}
		
		r <- raster(x)
		if (na.rm) {
			x <- sampleRandom(x, min(ncell(r), size), cells=TRUE, na.rm=TRUE)
			r <- rasterize(xyFromCell(r, x[,1]), r, x[,-1], ...)
		} else {
			cells <- sample(ncell(r), size)
			x <- extract(x, cells)
			r <- rasterize(xyFromCell(r, cells), r, x, ...)
		}
		return(r)
	}
	
	stopifnot(size <= ncell(x))
	
	r <- raster(x)
	nc <- ncell(r)
	
	layn <- names(x)

	removeCells <- FALSE
	if (sp | rowcol | xy) {
		removeCells <- ! cells
		cells <- TRUE
	}


	if ( inMemory(x) ) {
		if (is.null(ext)) {
			x <- getValues(x)
		} else {
			x <- crop(x, ext)
			x <- getValues(x)
		}
		
		if (cells) {
			if (is.null(ext)) {
				x <- cbind(cell=1:nc, value=x)			
			} else {
				xy <- xyFromCell(r, 1:nc)
				cell <- cellFromXY(r, xy)
				x <- cbind(cell, value=x)
			}
		}

		if (na.rm) { 
			x <- na.omit(x) 
		}

		if (is.matrix(x)) {
			d <- dim(x)
			x <- matrix(as.vector(x), d[1], d[2])
			if ( nrow(x) > size) {
				s = sampleInt(nrow(x), size)
				x <- x[s, ]
			}
		} else { 
			x <- as.vector(x)
			s <- sampleInt(length(x), size)
			x <- x[s]			
		}
		
	} else {
		
		if (! is.null(ext)) {
			r <- crop(r, ext)
		}
			
		if (size >= nc) {
			
			if (is.null(ext)) {
				x <- cbind(1:nc, getValues(x))
			} else {
				x <- cbind(1:nc, getValues(crop(x, ext)))			
			}
			
			if (cells) {
				if (is.null(ext)) {
					x <- cbind(cell=1:nc, value=x)
				} else {
					xy <- xyFromCell(r, 1:nc)
					cell <- cellFromXY(x, xy)
					x <- cbind(cell, value=x)
				}
			}
			if (na.rm) { 
				x <- na.omit(x) 
				if (is.matrix(x)) {
					d <- dim(x)
					x <- matrix(as.vector(x), d[1], d[2])
				} else {
					x <- as.vector(x)
				}
			}
						
		} else {	
		
			if (na.rm) {
				N <- 4 * size 
			} else {
				N <- size 
			}	
			
			N <- min(N, nc)
			rcells <- sampleInt(nc, N)
			
			if (!is.null(ext)) {
				xy <- xyFromCell(r, rcells)
				rcells <- cellFromXY(x, xy)
			}
			
			x <- .cellValues(x, rcells)
			if (cells) {
				x <- cbind(cell=rcells, value=x)
			}
			
			if (na.rm) {
				x <- na.omit(x)
				if (is.matrix(x)) {
					d <- dim(x)
					x <- matrix(as.vector(x), d[1], d[2])
					if (nrow(x) > size) {
						x <- x[1:size, ]
					}
				} else {
					x <- as.vector(x)
					if ( length(x) > size ) {
						x <- x[1:size]
					}
				}
			}	
		}
		
	} 

	if (is.matrix(x)) {
		if (cells) {
			colnames(x) <- c('cell', layn)
		} else {
			colnames(x) <- layn
		}
	}
	
	if (xy) {
		xy <- xyFromCell(r, x[,1])
		x <- cbind(x[,1,drop=FALSE], xy, x[,2:ncol(x),drop=FALSE])
	}
	if (rowcol) {
		x <- cbind(x[,1,drop=FALSE], rc, x[,2:ncol(x), drop=FALSE])
	}
	if (sp) {
		xy <- data.frame(xyFromCell(r, x[,1]))
		if (removeCells) {
			x <- x[,-1,drop=FALSE]
		}
		x <- SpatialPointsDataFrame(xy, data=data.frame(x), proj4string=projection(r, asText=FALSE))
		
	} else if (removeCells) {
		x <- x[,-1,drop=FALSE]	
	}
	
	return(x)
}
)


