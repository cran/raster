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
function(x, filename, options=NULL, format, ...) {

	filename <- trim(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	
	if (filetype=='ascii') {stop('ascii files not yet supported by this function, you can use writeRaster') }
	res <- filetype %in% c(.nativeDrivers())
	if (res) { 
		.startRasterWriting(x, filename, format=filetype, ...)
	} else {
		.startGDALwriting(x, filename, options, format=filetype, ...)
	}		
})

setMethod('writeStart', signature(x='RasterBrick', filename='character'), 
function(x, filename, options=NULL, format, ...) {

	filename <- trim(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)

	if (filetype=='ascii') {stop('ascii files cannot write multi-layer files') }
	native <- filetype %in% c(.nativeDrivers(), 'ascii')
	if (native) { 
		return( .startRasterWriting(x, filename, format=filetype, ...) )
	} else {
		return( .startGDALwriting(x, filename, options, format=filetype, ...) )
	}
})


setMethod('writeStop', signature(x='RasterLayer'), 
function(x) {
	native <- x@file@driver %in% c(.nativeDrivers(), 'ascii')
	if (native) { 
		return( .stopRasterWriting(x) )
	} else {
		return( .stopGDALwriting(x) )
	}
})

setMethod('writeStop', signature(x='RasterBrick'), 
function(x) {
	native <- x@file@driver %in% c(.nativeDrivers(), 'ascii')
	if (native) { 
		return( .stopRasterWriting(x) )
	} else {
		return( .stopGDALwriting(x) )
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

		native <- x@file@driver %in% c(.nativeDrivers(), 'ascii')
		
		if (native) {
			if (x@file@dtype == "INT" || x@file@dtype =='LOG' ) { 
				v <- as.integer(round(v))  
				v[is.na(v)] <- as.integer(x@file@nodatavalue)		
			} else { 
				v  <- as.numeric( v ) 
			}
			writeBin(v, x@file@con, size=x@file@dsize )
		} else {
			off = c(start-1, 0)
			v[is.na(v)] <- x@file@nodatavalue
			v = matrix(v, nrow=ncol(x))
			gd <- putRasterData(x@file@transient, v, band=1, offset=off) 	
			.Call("RGDAL_SetNoDataValue", gd, as.double(x@file@nodatavalue), PACKAGE = "rgdal")
		}
		return(invisible(NULL))
	} 		
)



setMethod('writeValues', signature(x='RasterBrick'), 
	function(x, v, start) {
		v[is.infinite(v)] <- NA
	
		native <- x@file@driver %in% .nativeDrivers()
		
		if (native) {
			if (x@file@dtype == "INT" || x@file@dtype =='LOG' ) { 
				v <- as.integer(round(v))  
				v[is.na(v)] <- as.integer(x@file@nodatavalue)		
			} else { 
				v  <- as.numeric( v ) 
			}
			writeBin(v, x@file@con, size=x@file@dsize )
		} else {
			nl <- nlayers(x)
			off = c(start-1, 0)
			for (i in 1:nl) {
				vv = matrix(v[,i], nrow=ncol(x))
				vv[is.na(vv)] <- x@file@nodatavalue
				gd <- putRasterData(x@file@transient, vv, band=i, offset=off) 	
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

