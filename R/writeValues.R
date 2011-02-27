# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

setMethod('writeValues', signature(x='RasterLayer'), 
	function(x, v, start) {

		v[is.infinite(v)] <- NA
		
		rsd <- na.omit(v) # min and max values
		if (length(rsd) > 0) {
			x@data@min <- min(x@data@min, rsd)
			x@data@max <- max(x@data@max, rsd)
		}	
		
		if ( x@file@driver == 'gdal' ) {
			off = c(start-1, 0)
			if (substr(x@file@datanotation,5,5) == 'U') {
				v[v < 0] <- NA
				if (x@file@datanotation == 'INT4U') {
					v[v > 2147483647] <- NA
				}
			}
			
			v[is.na(v)] <- x@file@nodatavalue
			v <- matrix(v, nrow=x@ncols)
			
			gd <- putRasterData(x@file@transient, v, band=1, offset=off) 	

		} else if ( x@file@driver %in% .nativeDrivers() ) {
			if (x@file@dtype == "INT" ) { 
				if (substr(x@file@datanotation, 5 , 5) == 'U') { 
					v[v < 0] <- NA
					if (x@file@datanotation == 'INT4U') { 
						v[is.na(v)] <- x@file@nodatavalue
						i <- v > 2147483647
						v[i] <- 2147483647 - v[i]
					}
				} else {
					v[is.na(v)] <- as.integer(x@file@nodatavalue)		
				}
				v <- as.integer(round(v))  
				
			} else if ( x@file@dtype =='LOG' ) {
				v[v != 1] <- 0
				v <- as.integer(v)  
				v[is.na(v)] <- as.integer(x@file@nodatavalue)		
			} else { 
				v  <- as.numeric( v ) 
			}
			start <- (start-1) * x@ncols * x@file@dsize
			seek(x@file@con, start, rw='w')			
			writeBin(v, x@file@con, size=x@file@dsize )
			
		} else if ( x@file@driver == 'netcdf') {

			x <- .writeValuesCDF(x, v, start)
			
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
			stop('huh? Was writeStart used?')
		}
		return(x)
	} 		
)



setMethod('writeValues', signature(x='RasterBrick'), 
	function(x, v, start) {
	
		v[is.infinite(v)] <- NA
		
		if ( x@file@driver %in% .nativeDrivers() ) {
			
			if (!is.matrix(v)) v <- matrix(v, ncol=1)

			if (x@file@dtype == "INT") { 
				v[is.na(v)] <- x@file@nodatavalue		
				dm <- dim(v)
				v <- as.integer(round(v))  
				dim(v) <- dm
			} else if ( x@file@dtype =='LOG' ) {
				v[v != 1] <- 0
				v[is.na(v)] <- x@file@nodatavalue
				dm <- dim(v)
				v <- as.integer(round(v))  
				dim(v) <- dm
			} else { 
				v[]  <- as.numeric( v ) 
			}

			w <- getOption('warn')
			options('warn'=-1) 
			rng <- apply(v, 2, range, na.rm=TRUE)
			x@data@min <- pmin(x@data@min, rng[1,])
			x@data@max <- pmax(x@data@max, rng[2,])
			options('warn'= w) 

			
			if (x@file@bandorder=='BIL') {
			
				start <- (start-1) * x@ncols * x@file@dsize * nlayers(x)
				seek(x@file@con, start, rw='w')			
				
				loop <- nrow(v) / x@ncols
				start <- 1
				for (i in 1:loop) {
					end <- start + x@ncols - 1
					writeBin(as.vector(v[start:end,]), x@file@con, size=x@file@dsize )
					start <- end + 1
				}
				
			} else if (x@file@bandorder=='BIP') {
			
				start <- (start-1) * x@ncols * x@file@dsize * nlayers(x)
				seek(x@file@con, start, rw='w')	
				writeBin(as.vector(t(v)), x@file@con, size=x@file@dsize )
				
			} else if (x@file@bandorder=='BSQ') {
			
				start <- (start-1) * x@ncols * x@file@dsize
				nc <- ncell(x) * x@file@dsize
				for (i in 1:ncol(v)) {
					pos <- start + nc * (i-1)
					seek(x@file@con, pos, rw='w')
					writeBin(v[,i], x@file@con, size=x@file@dsize )
				}
			} else {
				stop('unknown band order')
			}
			
		} else if ( x@file@driver == 'netcdf') {

			x <- .writeValuesBrickCDF(x, v, start)

		} else {
			off = c(start-1, 0)
			if (x@file@datanotation == 'INT1U') {
				v[v < 0] <- NA
			}
			v[is.na(v)] <- x@file@nodatavalue
			for (i in 1:nlayers(x)) {
				vv = matrix(v[,i], nrow=ncol(x))
				gd <- putRasterData(x@file@transient, vv, band=i, offset=off) 	
			}
		}
		return(x)
	}	
)


.getTransientRows <- function(x, r, n=1) {
	reg = c(n, ncol(x))
	off = c(r-1,0)
	as.vector((getRasterData(x@file@transient, region.dim=reg, offset=off)))
}

