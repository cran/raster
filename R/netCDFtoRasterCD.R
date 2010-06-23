# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.9
# Licence GPL v3


.getxvar <- function(xvar, vars) {
	if (xvar == '') {
		if ('x' %in% vars) { xvar <- 'x'
		} else if ('lon' %in% vars) { xvar <- 'lon' 
		} else if ('long' %in% vars) { xvar <- 'long' 
		} else if ('longitude' %in% vars) { xvar <- 'longitude' 
		} else if ('Longitude' %in% vars) { xvar <- 'Longitude' 
		} else { stop('Cannot find an obvious xvar in file. Select one from:\n', paste(vars, collapse=", "))  
		}
	} else if (!(xvar %in% vars)) { stop( paste('Cannot find obvious "xvar" in file. Select one from:\n', paste(vars, collapse=", "))) }	
	return(xvar)
}


.getyvar <- function(yvar, vars) {
	if (yvar == '') { 
		if ('y' %in% vars){ yvar <- 'y'
		} else if ('lat' %in% vars) { yvar <- 'lat' 
		} else if ('latitude' %in% vars) { yvar <- 'latitude' 
		} else if ('Latitude' %in% vars) { yvar <- 'Latitude' 
		} else { stop('Cannot find an obvious yvar in file. Select one from:\n', paste(vars, collapse=", "))  
		}
	} else if (!(yvar %in% vars)) { stop( paste('Cannot find obvious "yvar" in file. Select one from:\n', paste(vars, collapse=", "))) }	
	return(yvar)
}


.getVarname <- function(varname, vars) {
	if (varname == '') { varname <- 'value' }
	if (!(varname %in% vars)) { stop ( 'Cannot find an obvious "varname" in file. Select one from:\n', paste(vars, collapse=", ") ) }
	return(varname)
}


.rasterObjectFromCDF <- function(filename, x='', y='', varname='', band=NA, type='RasterLayer', ...) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package first') }
	nc <- open.nc(filename)
	conv <- 'CF'
	natt <- file.inq.nc(nc)$ngatts
	if (natt > 0) {
		for (i in 1:natt) {
			if (att.inq.nc(nc,"NC_GLOBAL", i-1)$name == 'Conventions') {
				conv <- att.get.nc(nc, "NC_GLOBAL", 'Conventions')
			}
		}
	}	
	if (substr(conv, 1, 3) == 'RST') {
		close.nc(nc)
		return( .rasterObjectFromCDFrst(filename, band=band, type='RasterLayer', ...) )
	}
	
	nv <- file.inq.nc(nc)$nvars
    vars <- vector()
	for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
	zvar <- .getVarname(varname, vars) 

	varinfo <- try(var.inq.nc(nc, zvar))
	
	datatype <- .getRasterDTypeFromCDF(varinfo$type)
	
	dims <- varinfo$ndims
	if (dims== 1) { 
		stop('"varname" only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		stop('"varname" has ', length(dims), ' dimensions, I do not know how to process this')
	}
	
	xvar <- .getxvar(x, vars) 
	yvar <- .getyvar(y, vars) 

	ncols <- dim.inq.nc(nc, xvar)$length
	nrows <- dim.inq.nc(nc, yvar)$length

	xx <- as.vector(var.get.nc(nc, xvar))
	rs <- xx[-length(xx)] - xx[-1]
	
	if (! isTRUE ( all.equal( min(rs), max(rs), scale= min(rs)/100 ) ) ) {
		stop('cells are not equally spaced; perhaps consider using these data as points') 
	}
	
	xrange <- c(min(xx), max(xx))
	resx <- (xrange[2] - xrange[1]) / (ncols-1)
	rm(xx)

	yy <- as.vector(var.get.nc(nc, yvar))
	rs <- yy[-length(yy)] - yy[-1]
	if (! isTRUE ( all.equal( min(rs), max(rs), scale= min(rs)/100 ) ) ) {
		stop('cells are not equally spaced; you should extract values as points') }
	yrange <- c(min(yy), max(yy))
	resy <- (yrange[2] - yrange[1]) / (nrows-1)

	if (yy[1] > yy[length(yy)]) { toptobottom  <- FALSE
	} else { toptobottom <- TRUE }

	rm(yy)

	xrange[1] <- xrange[1] - 0.5 * resx
	xrange[2] <- xrange[2] + 0.5 * resx
	yrange[1] <- yrange[1] - 0.5 * resy
	yrange[2] <- yrange[2] + 0.5 * resy
 
	att <- var.inq.nc(nc, variable=zvar)
	add_offset <- 0
	scale_factor <- 1
	long_name <- zvar
	missing_value <- NA
	projection <- NA
	if (att$natts > 0) {
		for (i in 0:(att$natts-1)) {
			if (att.inq.nc(nc, zvar, i)$name == "add_offset") {
				add_offset <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "scale_factor") {
				scale_factor <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "long_name") {
				long_name <- att.get.nc(nc, zvar, i)
			}
			if (att.inq.nc(nc, zvar, i)$name == "missing_value") {
				missing_value <- att.get.nc(nc, zvar, i)
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
		# now what?
		# projection(r) <- ...
		} 
	}
	
	if (type == 'RasterLayer') {
		r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
	} else {
		r <- brick(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
	}
	
	if (xrange[1] < -181 | xrange[2] > 181 | yrange[1] < -91 | yrange[2] > 91) {
		projection(r) <- NA
	}
	r@file@name <- filename
	r@file@toptobottom <- toptobottom
	r <- .enforceGoodLayerNames(r, long_name)

	attr(r@data, "xvar") <- xvar
	attr(r@data, "yvar") <- yvar
	attr(r@data, "zvar") <- zvar
	attr(r@data, "add_offset") <- add_offset
	attr(r@data, "scale_factor") <- scale_factor
	
	attr(r, "prj") <- prj 
	r@file@driver <- "netcdf"	
	if (! is.na(missing_value)) {
		r@file@nodatavalue <- missing_value
	}
	r@data@source <- 'disk'
	
	if (dims == 2) {
		nbands = 1
	} else {
		r@file@nbands <- as.integer(dim.inq.nc(nc, var.inq.nc(nc, zvar)$dimids[3])$length)
	}

	if (type == 'RasterLayer') {
		if (is.na(band) | is.null(band)) {
			if (length(dims)== 3) { 
				stop(zvar, 'has three dimensions, provide a "band" value between 1 and ', dims[3])
			} 
		} else {
			if (length(band) > 1) {
				stop('A RasterLayer can only have a single band. You can use a RasterBrick instead')
			}		
			if (is.na(band)) {
				r@data@band <- as.integer(1)
			} else {
				r@data@band <- as.integer( min(max(1, band), r@file@nbands) )
			}
		} 

	} else {
		if (length(dims)== 2) { 
			stop('cannot make a RasterStack or Brick from a data that has only two dimensions (no time step), use raster() instead, and then make a stack or brick from that')	
		} 
		r@data@nlayers <- r@file@nbands
	}
	close.nc(nc)
	return(r)
}

#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#p = .rasterObjectFromCDF(f, zvar='pr', type='RasterLayer', time=10)

