# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric('writeStart')) {
	setGeneric('writeStart', function(x, filename, ...)
		standardGeneric('writeStart')) 
}  

if (!isGeneric('writeStop')) {
	setGeneric('writeStop', function(x)
		standardGeneric('writeStop')) 
}  
	
if (!isGeneric('writeValues')) {
	setGeneric('writeValues', function(x, v, ...)
		standardGeneric('writeValues')) 
}  
	

setMethod('writeStart', signature(x='RasterLayer', filename='character'), 
function(x, filename, options=NULL, doPB=FALSE, ...) {
	filetype <- .filetype(...)
	filename <- .getExtension(filename, filetype)
	
	if (filetype=='ascii') {stop('ascii files not yet supported by this function, you can use writeRaster') }
	res <- filetype %in% c(.nativeDrivers())
	if (res) { 
		.startRasterWriting(x, filename, ...)
	} else {
		.startGDALwriting(x, filename, options, doPB, ...)
	}		
})

setMethod('writeStart', signature(x='RasterBrick', filename='character'), 
function(x, filename, options=NULL, doPB=FALSE, ...) {
	filetype <- .filetype(...)
	res <- filetype %in% c(.nativeDrivers(), 'ascii')
	if (res) { 
		.startRasterWriting(x, filename, ...)
	} else {
		.startGDALwriting(x, filename, options, doPB, ...)
	}
})

setMethod('writeStop', signature(x='RasterLayer'), 
function(x) {
	res <- x@file@driver %in% c(.nativeDrivers(), 'ascii')
	if (res) { 
		.stopRasterWriting(x)
	} else {
		.stopGDALwriting(x, doPB=TRUE)
	}
})

setMethod('writeStop', signature(x='RasterBrick'), 
function(x) {
	res <- x@file@driver %in% c(.nativeDrivers(), 'ascii')
	if (res) { 
		.stopRasterWriting(x)
	} else {
		.stopGDALwriting(x, doPB=TRUE)
	}
})


setMethod('writeValues', signature(x='RasterLayer'), 
	function(x, v, start) {
		v[is.infinite(v)] <- NA
		
		rsd <- na.omit(v) # min and max values
		if (length(rsd) > 0) {
			x@data@min <- min(x@data@min, rsd)
			x@data@max <- max(x@data@max, rsd)
		}	

		res <- x@file@driver %in% c(.nativeDrivers(), 'ascii')
		if (res) {
			if (x@file@dtype == "INT" || x@file@dtype =='LOG' ) { 
				v <- as.integer(round(v))  
				v[is.na(v)] <- as.integer(x@file@nodatavalue)		
			} else { 
				v  <- as.numeric( v ) 
			}
			writeBin(v, x@file@con, size=x@file@dsize )
		} else {
			off = c(start-1, 0)
			v = matrix(v, nrow=ncol(x))
			gd <- putRasterData(x@file@transient, v, band=1, offset=off) 	
		}
		return(invisible(NULL))
	} 		
)



setMethod('writeValues', signature(x='RasterBrick'), 
	function(x, v, start) {
		v[is.infinite(v)] <- NA
		
		res <- x@file@driver %in% c(.nativeDrivers(), 'ascii')
		
		if (res) {
			if (x@file@dtype == "INT" || x@file@dtype =='LOG' ) { 
				v <- as.integer(round(v))  
				v[is.na(v)] <- as.integer(x@file@nodatavalue)		
			} else { 
				v  <- as.numeric( v ) 
			}
			writeBin(v, x@file@con, size=x@file@dsize )
		} else {
			nl <- nlayers(raster)
			off = c(start-1, 0)
			for (i in 1:nl) {
				v = matrix(v[,i], nrow=ncol(x))
				gd <- putRasterData(x@file@transient, v, band=i, offset=off) 	
			}
		}
		return(invisible(NULL))
	}	
)


.getTransientRows <- function(x, r, n=1) {
	reg = c(n, ncol(x))
	off = c(r-1,0)
	as.vector((getRasterData(x@file@transient, region.dim=reg, offset=off)))
}

