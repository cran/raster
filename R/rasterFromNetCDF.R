# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.9
# Licence GPL v3


.isNetCDF <- function(x) {
	on.exit(options('warn'= getOption('warn')))
	options('warn'=-1) 
	fcon <- file(x, "rb")
	tst <- try( w <- readBin(fcon, what='character', n=1), silent=TRUE)
	close(fcon)
	if ( isTRUE((substr(w, 1, 3) == "CDF" ))) { return(TRUE) 
	} else { return(FALSE)
	}
}


.getxvar <- function(xvar, vars) {
	if (xvar == '') {
		if ('x' %in% vars) { xvar <- 'x'
		} else if ('lon' %in% vars) { xvar <- 'lon' 
		} else if ('long' %in% vars) { xvar <- 'long' 
		} else if ('longitude' %in% vars) { xvar <- 'longitude' 
		} else { stop('Cannot find an obvious xvar in file. Select one from:\n', paste(vars, collapse=", "))  
		}
	} else if (!(xvar %in% vars)) { stop( paste('Cannot find "xvar" in file. Select one from:\n', paste(vars, collapse=", "))) }	
	return(xvar)
}

.getyvar <- function(yvar, vars) {
	if (yvar == '') { if ('y' %in% vars){ yvar <- 'y'
		} else if ('lat' %in% vars) { yvar <- 'lat' 
		} else if ('latitude' %in% vars) { yvar <- 'latitude' 
		} else { stop('Cannot find an obvious yvar in file. Select one from:\n', paste(vars, collapse=", "))  
		}
	} else if (!(yvar %in% vars)) { stop( paste('Cannot find "yvar" in file. Select one from:\n', paste(vars, collapse=", "))) }	
	return(yvar)
}

.getzvar <- function(zvar, vars) {
	if (zvar == '') { zvar <- 'z' }
	if (!(zvar %in% vars)) { stop ( 'Cannot find an obvious "zvar" in file. Select one from:\n', paste(vars, collapse=", ") ) }
	return(zvar)
}

.nctoraster <- function(nc, vars, xvar, yvar) {
	xvar <- .getxvar(xvar, vars) 
	yvar <- .getyvar(yvar, vars) 
	# to do: also consider "lat_bnds" and "lat_bnds"
	
	ncols <- dim.inq.nc(nc, xvar)$length
	nrows <- dim.inq.nc(nc, yvar)$length

	xx <- as.vector(var.get.nc(nc, xvar))
	rs <- xx[-length(xx)] - xx[-1]
	if (! isTRUE ( all.equal( min(rs), max(rs), scale= min(rs)/100 ) ) ) {
		stop('cells are not equally spaced; extract as points') }
	xrange <- c(min(xx), max(xx))
	resx <- (xrange[2] - xrange[1]) / (ncols-1)
	rm(xx)

	yy <- as.vector(var.get.nc(nc, yvar))
	rs <- yy[-length(yy)] - yy[-1]
	if (! isTRUE ( all.equal( min(rs), max(rs), scale= min(rs)/100 ) ) ) {
		stop('cells are not equally spaced; you should extract values as points') }
	yrange <- c(min(yy), max(yy))
	resy <- (yrange[2] - yrange[1]) / (nrows-1)

	if (yy[1] > yy[length(yy)]) {
		toptobottom  <- FALSE
	} else {
		toptobottom <- TRUE
	}

	rm(yy)

	xrange[1] <- xrange[1] - 0.5 * resx
	xrange[2] <- xrange[2] + 0.5 * resx
	yrange[1] <- yrange[1] - 0.5 * resy
	yrange[2] <- yrange[2] + 0.5 * resy
    r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)

	if (xrange[1] < -400 | xrange[2] > 400 | yrange[1] < -100 | yrange[2] > 100) {
		projection(r) <- NA
	}
	
	r@file@toptobottom <- toptobottom
    return(r)
}


.rasterFromCDF <- function(filename, type, xvar='', yvar='', zvar='', time=NA, ...) {

# to be improved for large files (i.e. do not read all data from file...)
	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package first') }

	nc <- open.nc(filename)
	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
	zvar <- .getzvar(zvar, vars) 
	r <- .nctoraster(nc, vars, xvar, yvar)
	
	att <- var.inq.nc(nc, variable=zvar)
	
	add_offset <- 0
	scale_factor <- 1
	missing_value <- NA
	projection <- NA
	long_name <- zvar
	if (att$natts > 0) {
		for (i in 0:(att$natts-1)) {
			if (att.inq.nc(nc, zvar, i)$name == "add_offset") {
				add_offset <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "scale_factor") {
				scale_factor <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "missing_value") {
				missing_value <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "long_name") {
				long_name <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "grid_mapping") {
				projection <- att.get.nc(nc, zvar, i)
			}
		}
	}
	prj = list()
	if (!is.na(projection)) {
		att <- var.inq.nc(nc, projection)
		if (att$natts > 0) {
			for (i in 0:(att$natts-1)) {
				prj[[i+1]] <- att.get.nc(nc, projection, i)
				names(prj)[i+1] <- att.inq.nc(nc, projection, i)$name
			}
		}	
	}
	r@file@driver <- "netcdf"

	if (type != 'RasterLayer' ) {
		b <- .stackCDF(nc, type, r, xvar, yvar, zvar, time, add_offset, scale_factor, missing_value, long_name, prj)
		return(b)		
	} else if (length(time) > 1) {
		stop("cannot make a RasterLayer for multiple time steps, use 'stack' or 'brick' instead")		
	}
	

	r <- .enforceGoodLayerNames(r, long_name)
	attr(r, "prj") <- prj 

	if (is.na(time) | is.null(time)) {
		d <- var.get.nc(nc, variable=zvar)
		dims <- dim(d)
		if (length(dims)== 1) { 
			stop('zvar only has a single dimension')
		} else if (length(dims)== 2) { 
			d <- as.vector(d)
		} else if (length(dims)== 3) { 
			stop('zvar has three dimensions, provide a value for "time", between 1 and ', dims[3])
		} else if (length(dims)>= 4) { 
			stop('zvar has ', length(dims), ' dimensions, I do not know how to process these data')
		}
		
	} else {
		start <- c(1, 1, time)
		r@file@nbands <- as.integer(dim.inq.nc(nc, var.inq.nc(nc, zvar)$dimids[3])$length)
		r@data@band <- as.integer(time)
		count <- c(ncol(r), nrow(r), 1)
		d <- as.vector ( var.get.nc(nc, variable=zvar, start=start, count=count) )
	} 
	close.nc(nc)


	if (!is.na(missing_value)) {
		d[d==missing_value] <- NA
	}
	d <- add_offset + d * scale_factor
	d <- matrix(d, ncol=ncol(r), nrow=nrow(r), byrow=TRUE)
	
	if ( r@file@toptobottom ) { 
		d <- as.vector( t( d ) )	
	} else {
		d <- as.vector( t( d[nrow(r):1,] ) )	
	}
	return( setValues(r, d) )
}

