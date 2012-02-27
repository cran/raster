# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("sampleRandom")) {
	setGeneric("sampleRandom", function(x, size, ...)
		standardGeneric("sampleRandom"))
}	


setMethod('sampleRandom', signature(x='Raster'), 
function(x, size, na.rm=TRUE, ext=NULL, cells=FALSE, rowcol=FALSE, sp=FALSE, asRaster=FALSE, ...) {

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
	layn <- layerNames(x)

	
	if (sp | rowcol) {
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
				x <- cbind(cell=1:ncell(r), value=x)			
			} else {
				xy <- xyFromCell(r, 1:ncell(r))
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
			
		if (size >= ncell(r)) {
			
			if (is.null(ext)) {
				x <- cbind(1:ncell(r), getValues(x))
			} else {
				x <- cbind(1:ncell(r), getValues(crop(x, ext)))			
			}
			
			if (cells) {
				if (is.null(ext)) {
					x <- cbind(cell=1:ncell(r), value=x)
				} else {
					xy <- xyFromCell(r, 1:ncell(r))
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

			rcells <- sampleInt(ncell(r), N)
			
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
	
	if (rowcol) {
		rc <- rowColFromCell(r, x[,-1])
		if (sp | !removeCells) {
			x <- cbind(x[,1], rc, x[,2:ncol(x)])
			colnames(x) <- c('cell', 'row', 'col', layn)
		} else {
			x <- cbind(rc, x[,2:ncol(x)])
			colnames(x)[3:ncol(x)] <- layn
		}
	}
	
	if (sp) {
		xy <- data.frame(xyFromCell(r, x[,1]))
		if (removeCells) {
			x <- x[,-1,drop=FALSE]
		}
		x <- SpatialPointsDataFrame(xy, data=data.frame(x))
	}
	
	return(x)
}
)


