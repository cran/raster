# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.readRowsNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1)) {

	if ( x@file@toptobottom ) { 
		row <- x@nrows - row - nrows + 2 
	}
	
	nc <- open.ncdf(x@file@name)
	on.exit( close.ncdf(nc) )
	
	zvar <- x@data@zvar
		
	if (nc$var[[zvar]]$ndims == 2) {
		start <- c(col, row)
		count <- c(ncols, nrows)
		d <- get.var.ncdf( nc,  varid=zvar,  start=start, count=count )

	} else if (nc$var[[zvar]]$ndims == 3) {
		start <- c(col, row, x@data@band)
		count <- c(ncols, nrows, 1)
		d <- get.var.ncdf(nc, varid=zvar, start=start, count=count)
		
	} else {
		if (x@data@dim3 == 4) {
			start <- c(col, row, x@data@level, x@data@band)
			count <- c(ncols, nrows, x@data@level, 1)
			d <- get.var.ncdf(nc, varid=zvar, start=start, count=count)
		} else {
			start <- c(col, row, x@data@band, x@data@level)
			count <- c(ncols, nrows, 1, x@data@level)
			d <- get.var.ncdf(nc, varid=zvar, start=start, count=count)
		}
	}
	

	#if (!is.na(x@file@nodatavalue)) { d[d==x@file@nodatavalue] <- NA }
	#d <- x@data@add_offset + d * x@data@scale_factor
	
	if (length(dim(d)) > 1) {
		if ( x@file@toptobottom ) { 
			d <- d[, ncol(d):1] 	
		}
	}
	d <- as.vector(d) 
	d[d == x@file@nodatavalue] <- NA
	return(d)	
}
	
	
	
.readRowsBrickNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), layer=1, n=nlayers(x)-layer+1) {
	
	if ( x@file@toptobottom ) { row <- x@nrows - row - nrows + 2	}
	
	navalue <- x@file@nodatavalue
	layer   =  min( max( round(layer), 1), nlayers(x))
	n =  min( max( round(n), 1), nlayers(x)-layer+1 )
	
	nc <- open.ncdf(x@file@name)
	on.exit( close.ncdf(nc) )

	zvar = x@data@zvar
	
	if (nc$var[[zvar]]$ndims == 4) {
		if (x@data@dim3 == 4) {
			start = c(col, row, x@data@level, layer)
			count = c(ncols, nrows, x@data@level, n)
		} else {
			start = c(col, row, layer, x@data@level)
			count = c(ncols, nrows, n, x@data@level)
		}		
	} else {
		start = c(col, row, layer)
		count = c(ncols, nrows, n)
	}
	d <- get.var.ncdf(nc, varid=zvar, start=start, count=count)
	

	#if (!is.na(x@file@nodatavalue)) { 	d[d==x@file@nodatavalue] <- NA	}
	#d <- x@data@add_offset + d * x@data@scale_factor
	
	if (nlayers(x) > 1) {
		dims = dim(d)
		v <- matrix(nrow=nrows*ncols, ncol=n)

		if (length(dims) == 3) {
			if ( x@file@toptobottom ) { 
				for (i in 1:n) {
					x <- d[,,i]
					v[,i] <- as.vector( x[, ncol(x):1] )
				}
			} else {
				dim(d) = c(dims[1] * dims[2], dims[3])
				d[d == x@file@nodatavalue] <- NA
				return(d)
			}
		} else if (length(dims) == 2) {
			if (nrows==1) {
				v <- d
			} else if (n==1) {
				if ( x@file@toptobottom ) { 
					v[] <- as.vector(d[,ncol(d):1])
				} else {
					v[] <- as.vector(d)				
				}
			} else if (ncols==1) {
				if ( x@file@toptobottom ) { 
					d <- d[nrow(d):1,]
				}
				v <- d
			}
		} else {
			if ( x@file@toptobottom & nrows > 1) {
				d <- rev(d)
			}
			v[] <- d
		}
	} else {
		if ( x@file@toptobottom ) { 
			v <- as.vector( d[, ncol(d):1] )
		} else {
			v <- as.vector(d)
		}
		v = matrix(v, ncol=1)
	}
	
	v[v == navalue] <- NA
	return(v)
}


#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#r = .rasterFromCDF(f, zvar='pr', type='RasterLayer', time=1)
#q = .rasterReadAllCDF(r)

#b = .rasterFromCDF(f, zvar='pr', type='RasterBr', time=10)
#bd = .brickReadAllCDF(b)
