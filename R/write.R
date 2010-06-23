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
	
	if (filetype=='ascii') { 
		return(.startAsciiWriting(x, filename, ...)) 
	} else if ( filetype %in% .nativeDrivers() ) { 
		.startRasterWriting(x, filename, format=filetype, ...)
	} else {
		.startGDALwriting(x, filename, options=options, format=filetype, ...)
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
		return( .startGDALwriting(x, filename, options=options, format=filetype, ...) )
	}
})


setMethod('writeStop', signature(x='RasterLayer'), 
function(x) {
	if ( x@file@driver %in% .nativeDrivers() ) { 
		return( .stopRasterWriting(x) )
	} else if ( x@file@driver == 'ascii' ) { 
		return( .stopAsciiWriting(x) )
	} else {
		return( .stopGDALwriting(x) )
	}
})

setMethod('writeStop', signature(x='RasterBrick'), 
function(x) {
	native <- x@file@driver %in% c(.nativeDrivers())
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
		
		if ( x@file@driver %in% .nativeDrivers() ) {
			if (x@file@dtype == "INT" ) { 
				v <- as.integer(round(v))  
				v[is.na(v)] <- as.integer(x@file@nodatavalue)		
			} else if ( x@file@dtype =='LOG' ) {
				v[v != 1] <- 0
				v <- as.integer(v)  
				v[is.na(v)] <- as.integer(x@file@nodatavalue)		
			} else { 
				v  <- as.numeric( v ) 
			}
			writeBin(v, x@file@con, size=x@file@dsize )
			
		} else if ( x@file@driver == 'ascii') {
			opsci = options('scipen')
			if (x@file@dtype == 'INT') {
				options(scipen=10)
				v <- round(v)				
			}
			v[is.na(v)] <- x@file@nodatavalue
			v <- matrix(v, ncol=ncol(x), byrow=TRUE)
			write.table(v, x@file@name, append = TRUE, quote = FALSE, sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
			options(scipen=opsci)

		} else {
			off = c(start-1, 0)
			v[is.na(v)] <- x@file@nodatavalue
			v = matrix(v, nrow=ncol(x))
			gd <- putRasterData(x@file@transient, v, band=1, offset=off) 	
#			.Call("RGDAL_SetNoDataValue", gd, as.double(x@file@nodatavalue), PACKAGE = "rgdal")
		}
		return(invisible(x))
	} 		
)



setMethod('writeValues', signature(x='RasterBrick'), 
	function(x, v, start) {
	
		v[is.infinite(v)] <- NA
		
		if ( x@file@driver %in% .nativeDrivers() ) {
			
			if (x@file@dtype == "INT") { 
				v[is.na(v)] <- x@file@nodatavalue		
				v[] <- as.integer(round(v))  
			} else if ( x@file@dtype =='LOG' ) {
				v[v != 1] <- 0
				v[is.na(v)] <- x@file@nodatavalue
				v[] <- as.integer(v)  
			} else { 
				v[]  <- as.numeric( v ) 
			}


			w <- getOption('warn')
			options('warn'=-1) 
			rng <- apply(v, 2, range, na.rm=TRUE)
			x@data@min <- pmin(x@data@min, rng[1,])
			x@data@max <- pmax(x@data@max, rng[2,])
			options('warn'= w) 

		
			loop <- nrow(v) / x@ncols
			start <- 1
			for (i in 1:loop) {
				end <- start + x@ncols - 1
				writeBin(as.vector(v[start:end,]), x@file@con, size=x@file@dsize )
				start <- end + 1
			}
			
		} else {
			nl <- nlayers(x)
			off = c(start-1, 0)
			for (i in 1:nl) {
				vv = matrix(v[,i], nrow=ncol(x))
				vv[is.na(vv)] <- x@file@nodatavalue
				gd <- putRasterData(x@file@transient, vv, band=i, offset=off) 	
			}
		}
		return(invisible(x))
	}	
)


.getTransientRows <- function(x, r, n=1) {
	reg = c(n, ncol(x))
	off = c(r-1,0)
	as.vector((getRasterData(x@file@transient, region.dim=reg, offset=off)))
}

