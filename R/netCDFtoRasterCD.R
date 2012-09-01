# Author: Robert J. Hijmans
# Date: Aug 2009
# Version 0.9
# Licence GPL v3
# Aug 2011, adapted for use with ncdf4 (or ncdf) libraries


.NCDFversion4 <- function() {


	loadNCDF <- function() {
		if (!require(ncdf)) {
			stop('To open ncdf files, you need to first install package "ncdf" or "ncdf4"') 
		}
		options(rasterNCDF4 = FALSE)
		return(FALSE)
	}
	
	ncdf4 <- getOption('rasterNCDF4')

	if (is.null(ncdf4)) {
		if (length(find.package("ncdf4", quiet=TRUE)) > 0) {
			if (require(ncdf4, quietly=TRUE)) {
				options(rasterNCDF4 = TRUE)
				ncdf4 <- TRUE
				
			} else {
				ncdf4 <- loadNCDF()
			}
			
		} else {
			ncdf4 <- loadNCDF()
		}
	}
	return(ncdf4)
}



.doTime <- function(x, nc, zvar, dim3, ncdf4) {
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
		time <- startTime + as.numeric(getZ(x)) * 3600
		time <- as.character(time)
		if (!is.na(time[1])) {
			x@z <- list(time)
			names(x@z) <- as.character('Date/time')
		}
	}
	if (dodays) {
		# cal = nc$var[[zvar]]$dim[[dim3]]$calendar ?
		if (ncdf4) {
			cal <- ncdf4::ncatt_get(nc, "time", "calendar")$value
		} else {
			cal <- att.get.ncdf(nc, "time", "calendar")$value		
		}

		
		
		if (cal =='gregorian' | cal =='proleptic_gregorian' | cal=='standard') {
			greg <- TRUE
		} else if (cal == 'noleap' | cal == '365 day' | cal == '365_day') { 
			greg <- FALSE
			nday <- 365
		} else if (cal == '360_day') { 
			greg <- FALSE
			nday <- 360
		} else {
			greg <- TRUE
			warning('assuming a standard calender:', cal)
		}

		time <- getZ(x)
		if (greg) {
			time <- as.Date(time, origin=startDate)
		} else {
			startyear <-  as.numeric( format(startDate, "%Y") )
			startmonth <- as.numeric( format(startDate, "%m") )
			startday <- as.numeric( format(startDate, "%d") )
			year <- trunc( as.numeric(time)/nday )
			doy <- (time - (year * nday))
			origin <- paste(year+startyear, "-", startmonth, "-", startday, sep='')
			time <- as.Date(doy, origin=origin)		
		}
		x@z <- list(time)
		names(x@z) <- as.character('Date')
		
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
			a <- NULL
			for (i in 1:nv) { 
				a <- c(a, nc$var[[i]]$ndims) 
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

	ncdf4 <- .NCDFversion4()

	if (ncdf4) {
		options(rasterNCDF4 = TRUE)
		nc <- ncdf4::nc_open(filename)
		on.exit( ncdf4::nc_close(nc) )		
		conv <- ncdf4::ncatt_get(nc, 0, "Conventions")
		
	} else {
		options(rasterNCDF4 = FALSE)
		nc <- open.ncdf(filename)
		on.exit( close.ncdf(nc) )		
		conv <- att.get.ncdf(nc, 0, "Conventions")
	} 
	
	
	# assuming "CF-1.0"
	
	zvar <- .varName(nc, varname, warn=warn)
	
	datatype <- .getRasterDTypeFromCDF( nc$var[[zvar]]$prec )
	
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
	
	
	if (ncdf4) {
		a <- ncdf4::ncatt_get(nc, zvar, "long_name")
		if (a$hasatt) { long_name <- a$value }
		a <- ncdf4::ncatt_get(nc, zvar, "units")
		if (a$hasatt) { unit <- a$value }
		a <- ncdf4::ncatt_get(nc, zvar, "grid_mapping")
		if ( a$hasatt ) { projection  <- a$value }
		natest <- ncdf4::ncatt_get(nc, zvar, "_FillValue")
		natest2 <- ncdf4::ncatt_get(nc, zvar, "missing_value")		
		
	} else {
		a <- att.get.ncdf(nc, zvar, "long_name")
		if (a$hasatt) { long_name <- a$value }
		a <- att.get.ncdf(nc, zvar, "units")
		if (a$hasatt) { unit <- a$value }
		a <- att.get.ncdf(nc, zvar, "grid_mapping")
		if ( a$hasatt ) { projection  <- a$value }
		natest <- att.get.ncdf(nc, zvar, "_FillValue")
		natest2 <- att.get.ncdf(nc, zvar, "missing_value")		
	}

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
		names(r) <- long_name
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
	r@data@unit <- unit
	
	
	attr(r@data, "zvar") <- zvar
	attr(r@data, "dim3") <- dim3
	attr(r@data, "level") <- level
	
	attr(r, "prj") <- prj 
	r@file@driver <- "netcdf"	
	
	if (natest$hasatt) { 
		r@file@nodatavalue <- as.numeric(natest$value)
	} else if (natest2$hasatt) { 
		r@file@nodatavalue <- as.numeric(natest2$value)
	}
	r@data@fromdisk <- TRUE
	
	if (dims == 2) {
		nbands = 1
	} else {
		r@file@nbands <- nc$var[[zvar]]$dim[[dim3]]$len
		r@z <- list( nc$var[[zvar]]$dim[[dim3]]$vals )
		if ( nc$var[[zvar]]$dim[[dim3]]$name == 'time' ) {
			try( r <- .doTime(r, nc, zvar, dim3, ncdf4) )
		} else {
			names(r@z) <- nc$var[[zvar]]$dim[[dim3]]$units
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
			r@z <- list( getZ(r)[r@data@band] )
		} 

	} else {
		#if (length(dims)== 2) { 
		#	stop('cannot make a RasterBrick from data that has only two dimensions (no time step), use raster() instead, and then make a RasterBrick from that')	
		#} 
		r@data@nlayers <- r@file@nbands
		r@data@min <- rep(Inf, r@file@nbands)
		r@data@max <- rep(-Inf, r@file@nbands)
		try( names(r) <- as.character(r@z[[1]]), silent=TRUE )
	}
	
	return(r)
}

#f = "G:/cmip/ipcc/20c3m/atm/mo/pr/bccr_bcm2_0/run1/pr_A1_1.nc"
#p = .rasterObjectFromCDF(f, zvar='pr', type='RasterLayer', time=10)
