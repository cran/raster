# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("sampleRandom")) {
	setGeneric("sampleRandom", function(x, size, ...)
		standardGeneric("sampleRandom"))
}	


setMethod('sampleRandom', signature(x='Raster'), 
function(x, size, na.rm=TRUE, extent=NULL, cells=FALSE, rowcol=FALSE, sp=FALSE, ...) {

	r <- raster(x)
	layn <- layerNames(x)
	layn[layn==""] <- "value"
	
	if (sp | rowcol) {
		removeCells <- ! cells
		cells <- TRUE
	}

	if (!hasValues(x)) {
		stop('No values associated with the Raster object')
	}	
	
	if ( inMemory(x) ) {
		if (is.null(extent)) {
			x <- getValues(x)
		} else {
			x <- crop(x, extent)
			x <- getValues(x)
		}
		
		if (cells) {
			if (is.null(extent)) {
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
			if (length(x) > size ) {
				s = sampleInt(length(x), size)
				x <- x[s]			
			}
		}
		
	} else {
		
		if (! is.null(extent)) {
			r <- crop(r, extent)
		}
			
		if (size >= ncell(r)) {
			
			if (is.null(extent)) {
				x <- cbind(1:ncell(r), getValues(x))
			} else {
				x <- cbind(1:ncell(r), getValues(crop(x, extent)))			
			}
			
			if (cells) {
				if (is.null(extent)) {
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
			
			if (!is.null(extent)) {
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


