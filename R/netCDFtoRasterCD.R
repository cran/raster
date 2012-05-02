# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: Aug 2009
# Version 0.9
# Licence GPL v3


.doTime <- function(x, nc, zvar, dim3) {
	dodays <- TRUE
	dohours <- FALSE
	
	un <- nc$var[[zvar]]$dim[[dim3]]$units	
	if (substr(un, 1, 10) == "days since") { 
		startDate = as.Date(substr(un, 12, 22))
	} else {
		if (substr(un, 1, 11) == "hours since") { 
			dohours <- TRUE
		}
		dodays <- FALSE
	}
	if (dohours) {
		startTime <- substr(un, 13, 30)
		startTime <- strptime(startTime, "%Y-%m-%d %H:%M:%OS")
		time <- startTime + as.numeric(x@zvalue) * 3600
		time <- as.character(time)
		if (!is.na(time[1])) {
			x@zvalue <- time
			x@zname <- as.character('Date/time')
		}
	}
	if (dodays) {
		# cal = nc$var[[zvar]]$dim[[dim3]]$calendar ?
		cal = att.get.ncdf(nc, "time", "calendar")$value
		if (cal =='gregorian' | cal =='proleptic_gregorian' | cal=='standard') {
			greg <- TRUE
		} else if (cal == 'noleap' | cal == '365 day' | cal == '365_day') { 
			greg <- FALSE
		} else {
			greg <- TRUE
			warning('assuming a standard calender')
		}

		time <- x@zvalue
		if (greg) {
			time <- as.Date(time, origin=startDate)
		} else {
			a <- as.numeric(time)/365
			year <- trunc(a)
			doy <- (time - (year * 365))
			time <- as.Date(doy, origin=paste(year, "-1-1", sep='')) - 1
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


.varName <- function(nc, varname='', warn=TRUE) {
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
			if (warn) {
				warning('varname used is: ', varname, '\nIf that is not correct, set it to one of: ', paste(vars, collapse=", ") )
			}
		}
	}

	zvar <- which(varname == vars)
	if (length(zvar) == 0) {
		stop('varname: ', varname, ' does not exist in the file. Select one from:\n', paste(vars, collapse=", ") )
	}
	return(varname)
}


.rasterObjectFromCDF <- function(filename, varname='', band=NA, type='RasterLayer', lvar=3, level=0, warn=TRUE, ...) {

	if (!require(ncdf)) { stop('You need to install the ncdf package first') }
	nc <- open.ncdf(filename)
	on.exit( close.ncdf(nc) )
	
	conv <- att.get.ncdf(nc, 0, "Conventions")
	# assuming "CF-1.0"
	
	zvar <- raster:::.varName(nc, varname, warn=warn)
	
	datatype <- raster:::.getRasterDTypeFromCDF( nc$var[[zvar]]$prec )
	
	dim3 <- 3
	dims <- nc$var[[zvar]]$ndims
	if (dims== 1) { 
		stop(zvar, ' only has a single dimension; I cannot make a RasterLayer from this')
	} else if (dims == 4) { 
		if (type != 'RasterQuadBrick') {
			nlevs <- nc$var[[zvar]]$dim[[lvar]]$len
			if (level <=0 ) {
				level <- 1
				if (nlevs > 1) {
					warning('"level" set to 1 (there are ', nlevs, ' levels)')
				}
			} else {
				oldlevel <- level <- round(level)
				level <- max(1, min(level, nlevs))
				if (oldlevel != level) {
					warning('level set to: ', level)
				}
			}
			if (lvar == 4) { 
				dim3 <- 3 
			} else { 
				dim3 <- 4 
			}
		}
	} else if (dims > 4) { 
		warning(zvar, ' has more than 4 dimensions, I do not know what to do')
	}
	
	ncols <- nc$var[[zvar]]$dim[[1]]$len
	nrows <- nc$var[[zvar]]$dim[[2]]$len

	xx <- nc$var[[zvar]]$dim[[1]]$vals
	rs <- xx[-length(xx)] - xx[-1]
	
	if (! isTRUE ( all.equal( min(rs), max(rs), tolerance=0.025, scale=min(rs) ) ) ) {
		stop('cells are not equally spaced; perhaps consider using these data as points') 
	}
	
	xrange <- c(min(xx), max(xx))
	resx <- (xrange[2] - xrange[1]) / (ncols-1)
	rm(xx)

	
	yy <- nc$var[[zvar]]$dim[[2]]$vals
	rs <- yy[-length(yy)] - yy[-1]
	if (! isTRUE ( all.equal( min(rs), max(rs), tolerance=0.025, scale= min(rs)) ) ) {
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
	prj <- list()
	if (!is.na(projection)) {
		att <- nc$var[[projection]]
		prj <- as.list(unlist(att))
		# now parse .....
		# projection(r) <- ...
	}

	
	if (((tolower(substr(nc$var[[zvar]]$dim[[1]]$name, 1, 3)) == 'lon')  &
		(tolower(substr(nc$var[[zvar]]$dim[[2]]$name, 1, 3)) == 'lat')) | 
		(xrange[1] < -181 | xrange[2] > 181 | yrange[1] < -91 | yrange[2] > 91)) {
			crs <- '+proj=longlat +datum=WGS84'
	} else {
		crs <- NA
	}

		
	if (type == 'RasterLayer') {
		r <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows, crs=crs)
		layerNames(r) <- long_name
	} else if (type == 'RasterBrick') {
		r <- brick(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows, crs=crs)
		r@title <- long_name
	} else if (type == 'RasterQuadBrick') {
		r <- .quad(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], ncols=ncols, nrows=nrows, crs=crs)
		r@title <- long_name	
		if (lvar == 4) { 
			dim3 <- 3 
			step3 <- 4
		} else { 
			dim3 <- 4 
			step3 <- 3
		}
		r@nlevels <- nc$var[[zvar]]$dim[[dim3]]$len
		r@steps  <- nc$var[[zvar]]$dim[[step3]]$len
	}
	
	r@file@name <- filename
	r@file@toptobottom <- toptobottom
	r@unit <- unit
	
	
	attr(r@data, "zvar") <- zvar
	attr(r@data, "dim3") <- dim3
	attr(r@data, "level") <- level
	
	attr(r, "prj") <- prj 
	r@file@driver <- "netcdf"	
	
	natest <- att.get.ncdf(nc, zvar, "_FillValue")
	if (natest$hasatt) { 
		r@file@nodatavalue <- as.numeric(natest$value)
	} else {
		natest <- att.get.ncdf(nc, zvar, "missing_value")
		if (natest$hasatt) { 
			r@file@nodatavalue <- as.numeric(natest$value)
		}
	}
	r@data@fromdisk <- TRUE
	
	if (dims == 2) {
		nbands = 1
	} else {
		r@file@nbands <- nc$var[[zvar]]$dim[[dim3]]$len
		r@zname <- nc$var[[zvar]]$dim[[dim3]]$units
		r@zvalue <- nc$var[[zvar]]$dim[[dim3]]$vals
		
		if ( nc$var[[zvar]]$dim[[dim3]]$name == 'time' ) {
			r <- try( .doTime(r, nc, zvar, dim3)  )
		}
	}
	
	if (type == 'RasterLayer') {
		if (is.na(band) | is.null(band)) {
			if (dims > 2) { 
				stop(zvar, ' has mutliple layers, provide a "band" value between 1 and ', dims[dim3])
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
		#if (length(dims)== 2) { 
		#	stop('cannot make a RasterBrick from data that has only two dimensions (no time step), use raster() instead, and then make a RasterBrick from that')	
		#} 
		r@data@nlayers <- r@file@nbands
		r@data@min <- rep(Inf, r@file@nbands)
		r@data@max <- rep(-Inf, r@file@nbands)
		try( layerNames(r) <- as.character(r@zvalue), silent=TRUE )
	}
	
	return(r)
}

#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#p = .rasterObjectFromCDF(f, zvar='pr', type='RasterLayer', time=10)
