# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.readRowsNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1)) {

	if ( x@file@toptobottom ) { row <- x@nrows - row - nrows + 2	}
	
	nc <- open.nc(x@file@name)
	zvar = x@data@zvar

	if (file.inq.nc(nc)$ndims == 2) {
		start = c(col, row)
		count = c(ncols, nrows)
		d <- var.get.nc( nc,  variable=zvar,  start=start, count=count )

	} else {
		start = c(col, row, x@data@band)
		count = c(ncols, nrows, 1)
		d <- var.get.nc(nc, variable=zvar, start=start, count=count)
	}
	close.nc(nc)	

	if (!is.na(x@file@nodatavalue)) { 
		d[d==x@file@nodatavalue] <- NA
	}
	d <- x@data@add_offset + d * x@data@scale_factor
	
	if (length(dim(d)) > 1) {
		if ( x@file@toptobottom ) { 
			d <- d[, ncol(d):1] 	
		}
	}
	return( as.vector(d) )
	
}
	
	
	
.readRowsBrickNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), layer=1, nlayers=nlayers(x)-layer+1) {
	
	if ( x@file@toptobottom ) { row <- x@nrows - row - nrows + 2	}
	
	layer   =  min( max( round(layer), 1), nlayers(x))
	nlayers =  min( max( round(nlayers), 1), nlayers(x)-layer+1 )
	
	nc <- open.nc(x@file@name)
	zvar = x@data@zvar
	start = c(col, row, layer)
	count = c(ncols, nrows, nlayers)
	d <- var.get.nc(nc, variable=zvar, start=start, count=count)
	close.nc(nc)
	

	if (!is.na(x@file@nodatavalue)) { 
		d[d==x@file@nodatavalue] <- NA
	}
	d <- x@data@add_offset + d * x@data@scale_factor
	
	dims = dim(d)
	if (length(dims) == 3) {
		if ( x@file@toptobottom ) { 
			values <- matrix(nrow=nrows*ncols, ncol=nlayers)
			for (i in 1:nlayers) {
				x <- d[,,i]
				values[,i] <- as.vector( x[, ncol(x):1] )
			}
		} else {
			dim(d) = c(dims[1] * dims[2], dims[3])
			return(d)
		}
	} else {
		if ( x@file@toptobottom ) { 
			values <- as.vector( d[, ncol(d):1] )
		} else {
			values <- as.vector(d)
		}
		values = matrix(values, ncol=1)
	}
	return(values)
}


#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#r = .rasterFromCDF(f, zvar='pr', type='RasterLayer', time=1)
#q = .rasterReadAllCDF(r)

#b = .rasterFromCDF(f, zvar='pr', type='RasterBr', time=10)
#bd = .brickReadAllCDF(b)
