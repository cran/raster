# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3



setReplaceMethod("[", c("RasterLayer", "ANY", "missing"),
	function(x, i, j, value) {
		
		if (dataContent(x) != 'all') {
			if (canProcessInMemory(x, 2)) {
				if (dataSource(x) == 'disk') {
					x <- readAll(x)
				} else {
					x <- setValues(x, rep(NA, times=ncell(x)))
				}
			}
		}
				
		if (dataContent(x) == 'all') {
			if  (missing(i)) {
				if (length(value) == ncell(x)) {
					return(setValues(x, value))
				} else if (length(value) == 1) {
					return( setValues(x, rep(value, times=ncell(x))) )
				} else {
					v <- vector(length=ncell(x))
					v[] <- value
					return( setValues(x, v) )
				}
			}
		
			if (class(i) == "RasterLayer") {
				i <- as.logical( getValues(i) ) 
			}
			if (!is.logical(i)) {
				i <- subset(i, i <= ncell(x))
				i <- subset(i, i >= 1)
			}
			x@data@values[i] <- value
			x@data@source <- 'ram'
			filename(x) <- ""
			x <- setMinMax(x)
			return(x)
			
		} else {
		
			filename <- rasterTmpFile()
			outras <- raster(x)
			if  (missing(i)) { 
				i <- vector(length=ncol(outras))
				i[] <- TRUE
			}
			
			for (r in 1:nrow(outras)) {
				nrs <- cellFromRow(outras, r)
				start <- nrs[1]
				end <- nrs[length(nrs)]
				if (class(i) == "RasterLayer") {
					ind <- as.logical( getValues(i, r) ) 
				} else if (is.logical(i)) {
					if (length(i) == ncol(outras)) {
						ind <- i
					} else if (length(i) == ncell(outras)) {
						ind <- i[start:end]
					} else {
						stop('cannot recycle logical indices for large rasters')
					}
				} else {
					ind <- subset(i, i >= start)
					ind <- subset(ind, ind <= end)
				}
				if (length(ind) > 0) {
					if (length(value) == ncell(outras)) {
						val <- value[start:end]
					} else {
						val <- value
					}
				} else {
					if (length(value)==1) {
						val <- rep(value, times=ncol(outras))
					} else {
						val <- value
					}
				} 
				
				if (length(val) != ncol(outras) | length(ind) != ncol(outras)) {
					if (dataSource(x) == 'disk') {
						v <- getValues(x, r)
					} else {
						v <- rep(NA, ncol(outras))
					}
					v[ind] <- val
				} else {
					v <- val
				}
				outras <- setValues(outras, v, r)
				outras <- writeRaster(outras, filename)
			}
		return(outras)
		}
	}
)

