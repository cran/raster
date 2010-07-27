# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  Jaunary 2009
# Version 0.9
# Licence GPL v3


resample <- function(from, to, method="ngb", filename="", ...)  {
	
	if (!method %in% c('bilinear', 'ngb')) {
		stop('invalid method') 
	}
	if (method == 'ngb') method <- 'simple'
	
	filename <- trim(filename)
	
	if (inherits(from, 'RasterLayer')) {
		to <- raster(to)
	} else {
		to <- brick(to, values=FALSE)
	}

	e = intersectExtent(from, to, validate=TRUE)
	
	if (is.null(filename)){filename <- ""}
	
	if (!canProcessInMemory(to, 3) && filename == '') {
		filename <- rasterTmpFile()	
	}
	
	inMemory <- filename == ""
	if (inMemory) {
		v <- matrix(NA, nrow=ncell(to), nlayers(from))
	} else {
		to <- writeStart(to, filename=filename, ... )
	}

	rowCells <- 1:ncol(to)
	pb <- pbCreate(nrow(to), type=.progress(...))

	
	tr <- blockSize(to)
	pb <- pbCreate(tr$n, type=.progress(...))
	for (i in 1:tr$n) {
		r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
		xy <- xyFromCell(to, cellFromRowCol(to, tr$row[i], 1) : cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, ncol(to)) ) 

		vals <- xyValues(from, xy, method=method)

		if (inMemory) {
			start <- cellFromRowCol(to, tr$row[i], 1)
			end <- cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, to@ncols)
			v[start:end, ] <- vals
		} else {
			to <- writeValues(to, vals, tr$row[i])
		}

		pbStep(pb, i)
		
	}
	pbClose(pb)
	
	if (inMemory) {
		to <- setValues(to, v)
	} else {
		to <- writeStop(to)	
	}
	return(to)
}

