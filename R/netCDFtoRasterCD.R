# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.9
# Licence GPL v3


#.getxvar <- function(xvar, vars) {
#	if (xvar == '') {
#		if ('x' %in% vars) { xvar <- 'x'
#		} else if ('lon' %in% vars) { xvar <- 'lon' 
#		} else if ('long' %in% vars) { xvar <- 'long' 
#		} else if ('longitude' %in% vars) { xvar <- 'longitude' 
#		} else if ('Longitude' %in% vars) { xvar <- 'Longitude' 
#		} else { stop('Cannot find an obvious xvar in file. Select one from:\n', paste(vars, collapse=", "))  
#		}
#	} else if ( !(xvar %in% vars) ) { stop( paste(xvar, ' does not exist in the file. Select one from:\n', paste(vars, collapse=", "))) }	
#	return(xvar)
#}


#.getyvar <- function(yvar, vars) {
#	if (yvar == '') { 
#		if ('y' %in% vars){ yvar <- 'y'
#		} else if ('lat' %in% vars) { yvar <- 'lat' 
#		} else if ('latitude' %in% vars) { yvar <- 'latitude' 
#		} else if ('Latitude' %in% vars) { yvar <- 'Latitude' 
#		} else { stop('Cannot find an obvious yvar in file. Select one from:\n', paste(vars, collapse=", "))  
#		}
#	} else if (!(yvar %in% vars)) { stop( paste(yvar, ' does not exist in the file. Select one from:\n', paste(vars, collapse=", "))) }	
#	return(yvar)
#}


.doTime <- function(x, nc) {
	dodays <- TRUE
	dohours <- FALSE
	
	un = att.get.ncdf(nc, "time", "units")$value
	if (substr(un, 1, 10) == "days since") { 
		startDate = as.Date(substr(un, 12, 22))
	} else {
		if (substr(un, 1, 11) == "hours since") { 
			dohours <- TRUE
		}
		dodays <- FALSE
	}
	if (dohours) {
		startTime = substr(un, 13, 30)
		startTime = strptime(startTime, "%Y-%m-%d %H:%M:%OS")
		time <- startTime + as.numeric(x@zvalue) * 3600
		time <- as.character(time)
		if (!is.na(time[1])) {
			x@zvalue <- time
			x@zname <- as.character('Date/time')
		}
	}
	if (dodays) {
		cal = att.get.ncdf(nc, "time", "calendar")$value
		if (cal =='gregorian' | cal=='standard') {
			greg = TRUE
		} else if (cal == 'noleap' | cal == '365 day' | cal == '365_day') { 
			greg = FALSE
		} else {
			greg = TRUE
			warning('assuming a standard calender')
		}

		time <- x@zvalue
		if (greg) {
			time <- as.Date(time, origin=startDate)
		} else {
			a <- as.numeric(time)/365
			year <- trunc(a)
			doy <- (time - (year * 365))
			time <- as.Date(doy, origin=paste(year-1, "-12-31", sep=''))
		}
		x@zvalue <- as.character(time)
		x@zname <- as.character('Date')
		
	}
	return(x)
}



.dimNames <- function(nc) {
	n <- nc$dim
	nams <- vector(length=n)
	if (n > 0) {
		for (i in 1:n) {
			nams[i] <- nc$dim[[i]]$name
		}
	}
	return(nams)
}


.varName <- function(nc, varname='') {
	n <- nc$nvars
	vars <- vector(length=n)
	if (n > 0) {
		for (i in 1:n) {
			vars[i] <- nc$var[[i]]$name
		}
	}

	if (varname=='') { 
		nv <- length(vars)
		if (nv == 0) {
			stop()
		} 
		
		if (nv  == 1) {
			varname <- vars
		} else {
			# should also check its dimensions with those of x and y 
			a=NULL
			for (i in 1:nv) { 
				a = c(a, nc$var[[i]]$ndims) 
			}
			varname <- vars[which.max(a)]
			warning('varname used is: ', varname, '\nIf that is not correct, set it to one of: ', paste(vars, collapse=", ") )
		}
	}

	zvar <- which(varname == vars)
	if (length(zvar) == 0) {
		stop('varname: ', varname, ' does not exist in the file. Select one from:\n', paste(vars, collapse=", ") )
	}
	return(varname)
}


.rasterObjectFromCDF <- function(filename, x='', y='', varname='', band=NA, type='RasterLayer', ...) {

	if (!require(ncdf)) { stop('You need to install the ncdf package first') }
	nc <- open.ncdf(filename)
	on.exit( close.ncdf(nc) )
	
	conv <- 'CF'
	
	
	conv <- att.get.ncdf(nc, 0,  "Conventions")
	if (substr(conv$value, 1, 3) == 'RST') {
		close.ncdf(nc)
		return( .rasterObjectFromCDFrst(filename, band=band, type='RasterLayer', ...) )
	} else {
		# assuming "CF-1.0"
	}
	
	zvar <- .varName(nc, varname)
	
	datatype <- .getRasterDTypeFromCDF( nc$var[[zvar]]$prec )
	
	
	dims <- nc$var[[zvar]]$ndims
	if (dims== 1) { 
		stop('"varname" only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims > 3) { 
		stop('"varname" has ', length(dims), ' dimensions, I do not know how to process this')
	}
	
	ncols <- nc$var[[zvar]]$dim[[1]]$len
	nrows <- nc$var[[zvar]]$dim[[2]]$len

	xx <- nc$var[[zvar]]$dim[[1]]$vals
	rs <- xx[-length(xx)] - xx[-1]
	
	if (! isTRUE ( all.equal( min(rs), max(rs), scale= min(rs)/100 ) ) ) {
		stop('cells are not equally spaced; perhaps consider using these data as points') 
	}
	
	xrange <- c(min(xx), max(xx))
	resx <- (xrange[2] - xrange[1]) / (ncols-1)
	rm(xx)

	yy <- nc$var[[zvar]]$dim[[2]]$vals
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
 
	long_name <- zvar
	projection <- NA
	unit <- ''
	a <- att.get.ncdf(nc, zvar, "long_name")
	if (a$hasatt) { long_name <- a$value }
	a <- att.get.ncdf(nc, zvar, "units")
	if (a$hasatt) { unit <- a$value }
	a <- att.get.ncdf(nc, zvar, "grid_mapping")
	if ( a$hasatt ) { projection  <- a$value }

	prj = list()
	if (!is.na(projection)) {
		att <- nc$var[[projection]]
		prj <- as.list(unlist(att))
		# now parse .....
		# projection(r) <- ...
	}
		
	if (type == 'RasterLayer') {
		r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
		r <- .enforceGoodLayerNames(r, long_name)
	} else {
		r <- brick(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows)
		r@title <- long_name
	}
	
	if (xrange[1] < -181 | xrange[2] > 181 | yrange[1] < -91 | yrange[2] > 91) {
		projection(r) <- NA
	}
	r@file@name <- filename
	r@file@toptobottom <- toptobottom
	r@unit <- unit
	
	
#	attr(r@data, "xvar") <- xvar
#	attr(r@data, "yvar") <- yvar
	attr(r@data, "zvar") <- zvar
#	attr(r@data, "add_offset") <- add_offset
#	attr(r@data, "scale_factor") <- scale_factor
	
	attr(r, "prj") <- prj 
	r@file@driver <- "netcdf"	
	
	natest <- att.get.ncdf(nc, zvar, "_FillValue")
	if (natest$hasatt) { 
		r@file@nodatavalue <- natest$value
	}
	r@data@fromdisk <- TRUE
	
	if (dims == 2) {
		nbands = 1
	} else {
		r@file@nbands <- nc$var[[zvar]]$dim[[3]]$len
		r@zname <- nc$var[[zvar]]$dim[[3]]$units
		r@zvalue <- nc$var[[zvar]]$dim[[3]]$vals
		
		if ( nc$var[[zvar]]$dim[[3]]$name == 'time' ) {
			r <- .doTime(r, nc)
		}
	}
	
	if (type == 'RasterLayer') {
		if (is.na(band) | is.null(band)) {
			if (dims == 3) { 
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
			r@zvalue <- r@zvalue[r@data@band]
		} 

	} else {
		if (length(dims)== 2) { 
			stop('cannot make a RasterStack or RasterBrick from a data that has only two dimensions (no time step), use raster() instead, and then make a stack or brick from that')	
		} 
		r@data@nlayers <- r@file@nbands
		try( layerNames(r) <- r@zvalue, silent=TRUE )
	}
	
	return(r)
}

#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#p = .rasterObjectFromCDF(f, zvar='pr', type='RasterLayer', time=10)
