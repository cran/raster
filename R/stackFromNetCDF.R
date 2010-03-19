# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Sept 2009
# Version 0.9
# Licence GPL v3


.stackCDF <- function(nc, type, r, xvar, yvar, zvar, time, add_offset, scale_factor, missing_value, long_name, prj) {
# to be improved for large files (i.e. do not read all data from file...)

	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
	dd <- var.get.nc(nc, zvar)
    close.nc(nc)
	
	dims <- dim(dd)
	getRaster <- FALSE
	if (length(dims)== 3) { 
		if (is.numeric(time)) { 
			tsteps <- time	
		} else { 
			tsteps <- 1:dims[3] 
		}
		if (length(tsteps) < 2) { stop('cannot make a RasterStack or Brick from a single time step, use raster() instead, and then make a stack or brick from that') } 
		
	} else if (length(dims)== 2) { 
		stop('cannot make a RasterStack or Brick from a data that has only two dimensions (no time step), use raster() instead, and then make a stack or brick from that')	
	} else { 
		stop(paste('data has an unexpected number of dimensions', dims)) 
	}
	
	
#	for (i in tsteps) {

	d <- dd[,,tsteps]
	dims <- dim(d)
	if (!is.na(missing_value)) {
		d[d==missing_value] <- NA
	}
	d <- add_offset + d * scale_factor

	pb <- pbCreate(dims[3], type='text')
	
	if (type == 'RasterStack') {
		stk <- new('RasterStack')
		for (i in 1:dims[3]) {
			x <- t(d[,,i])
			x <- x[nrow(x):1, ]
			r[] <- as.vector(t(x))
			if (i==1) {
				stk <- stack(r)
			} else {
				stk@layers[[i]] <- r
			}
			pbStep(pb, i) 
		}
		attr(stk, 'prj') <- prj
		layerNames(stk) <- 1:nlayers(stk)
		pbClose(pb)		

		return(stk) 
	} else {
		b <- brick(r)
		b@data@values <- matrix(nrow=ncell(r), ncol=dims[3])
		for (i in 1:dims[3]) {
			x <- t(d[,,i])
			x <- x[nrow(x):1, ]
			b@data@values[,i] <- as.vector(t(x))
			pbStep(pb, i) 
		}
		pbClose(pb)		
		b@title <- long_name
		b@data@nlayers <- dims[3]
		b@data@content <- 'all'
		b@data@indices <- c(1:ncell(r))
		b@file@driver <- "netcdf"
		b <- setMinMax(b)
		attr(b, 'prj') <- prj
		layerNames(b) <- 1:nlayers(b)
		return(b)
	}
}

