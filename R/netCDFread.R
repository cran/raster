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
			count <- c(ncols, nrows, 1, 1)
			d <- get.var.ncdf(nc, varid=zvar, start=start, count=count)
		} else {
			start <- c(col, row, x@data@band, x@data@level)
			count <- c(ncols, nrows, 1, 1)
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
	
	
	
.readRowsBrickNetCDF <- function(x, row, nrows=1, col=1, ncols=(ncol(x)-col+1), lyrs) {
	
	if ( x@file@toptobottom ) { 
		row <- x@nrows - row - nrows + 2
	}
	navalue <- x@file@nodatavalue
	
	
	#n the true number of layers
	#nn the span of layers between the first and the last
	#alyrs, the layers requested, scaled to start at one.
	n <- nn <- nlayers(x)
	if (missing(lyrs)) {
		layer <- 1
		lyrs <- 1:n
	} else {
		lyrs <- lyrs[lyrs %in% 1:n]
		if (length(lyrs) == 0) {
			stop("no valid layers")
		}
		layer <- lyrs[1]
		n <- length(lyrs)
		nn <- lyrs[length(lyrs)] - lyrs[1] + 1
	}
	alyrs <- lyrs - lyrs[1] + 1
	lns <- names(x)[lyrs]
	
	nrows <- min(round(nrows), x@nrows-row+1)
	ncols <- min((x@ncols-col+1), ncols)
	stopifnot(nrows > 0)
	stopifnot(ncols > 0)

	
	nc <- open.ncdf(x@file@name)
	on.exit( close.ncdf(nc) )

	zvar = x@data@zvar
	
	if (nc$var[[zvar]]$ndims == 4) {
		if (x@data@dim3 == 4) {
			start <- c(col, row, x@data@level, layer)
			count <- c(ncols, nrows, 1, nn)
		} else {
			start <- c(col, row, layer, x@data@level)
			count <- c(ncols, nrows, nn, 1)
		}		
	} else {
		start <- c(col, row, layer)
		count <- c(ncols, nrows,  nn)
	}
	d <- get.var.ncdf(nc, varid=zvar, start=start, count=count)
	

	#if (!is.na(x@file@nodatavalue)) { 	d[d==x@file@nodatavalue] <- NA	}
	#d <- x@data@add_offset + d * x@data@scale_factor
	
	if (nlayers(x) > 1) {
		dims = dim(d)

		if (length(dims) == 3) {
			if ( x@file@toptobottom ) { 
				v <- matrix(nrow=nrows*ncols, ncol=n)
				for (i in 1:length(alyrs)) {
					x <- d[,,alyrs[i]]
					v[,i] <- as.vector( x[, ncol(x):1] )
				}
			} else {
				dim(d) = c(dims[1] * dims[2], dims[3])
				d <- d[, alyrs, drop=FALSE]
				d[d == x@file@nodatavalue] <- NA
				return(d)
			}
		} else if (length(dims) == 2) {
			if (nrows==1) {
				d <- d[ , alyrs,drop=FALSE]
				d[d == navalue] <- NA
				return(d)
				
			} else if (n==1) {
				v <- matrix(nrow=nrows*ncols, ncol=n)
				if ( x@file@toptobottom ) { 
					v[] <- as.vector(d[,ncol(d):1])
				} else {
					v[] <- as.vector(d)				
				}
				
			} else if (ncols==1) {
				if ( x@file@toptobottom ) { 
					d <- d[nrow(d):1, ]
				}
				d <- d[ , alyrs, drop=FALSE]
				d[d == navalue] <- NA
				return(d)
			}
		} else {
			v <- matrix(nrow=nrows*ncols, ncol=n)
			if ( x@file@toptobottom & nrows > 1) {
				d <- rev(d)
			}
			v[] <- d[, alyrs,drop=FALSE]
		}
	} else {
		if ( x@file@toptobottom ) { 
			v <- as.vector( d[, ncol(d):1] )
		} else {
			v <- as.vector(d)
		}
		v <- matrix(v, ncol=1)
		v <- v[,lyrs,drop=FALSE]
	}
	
	v[v == navalue] <- NA
	colnames(v) <- lns
	return(v)
}

