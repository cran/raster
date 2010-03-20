# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  Jaunary 2009
# Version 0.9
# Licence GPL v3


resample <- function(from, to, method="ngb", filename="", ...)  {
	
	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') 	}
	filename <- trim(filename)
	
	to <- raster(to)

	e = intersectExtent(from, to, validate=TRUE)
	
	if (is.null(filename)){filename <- ""}
	
	if (!canProcessInMemory(to, 1) && filename == '') {
		filename <- rasterTmpFile()
		
	}
	inMemory <- (filename == "")

	v <- vector(length=0)
	rowCells <- 1:ncol(to)

			
	pb <- pbCreate(nrow(to), type=.progress(...))

	
	
	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		if (method=='ngb') {
			vals <- xyValues(from, xy)
		} else {
			vals <- xyValues(from, xy, method='bilinear')
		}
		if (inMemory) {
			v <- c(v, vals)
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, filename, ...)
		}

		pbStep(pb, r)
	}
	pbClose(pb)

	if (inMemory) {
		to <- setValues(to, v) 
	}
	return(to)
}

