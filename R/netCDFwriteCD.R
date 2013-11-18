# Author: Robert J. Hijmans
# Date: June 2010
# Version 1.0
# Licence GPL v3



.startWriteCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE, att, 
		varname, varunit, varatt, longname, xname, yname, zname, zunit, zatt, NAflag, ...) {

		
	ncdf4 <- .NCDFversion4()
		
	filename = trim(filename)
	if (filename == '') { stop('provide a filename') }
	extension(filename) <- .defaultExtension(format='CDF')
	if (file.exists(filename) & !overwrite) {
		stop('file exists, use overwrite=TRUE to overwrite it')
	}
	dataType(x) <- datatype
	datatype = .getNetCDFDType(datatype)
	nl <- nlayers(x)
	
	if (.couldBeLonLat(x)) {
		if (missing(xname)) xname = 'longitude'
		if (missing(yname)) yname = 'latitude'
		xunit = 'degrees_east'
		yunit = 'degrees_north'
	} else {
		if (missing(xname)) xname = 'easting'
		if (missing(yname)) yname = 'northing'
		xunit = 'meter' # probably
		yunit = 'meter' # probably
	}
	
	if (missing(zunit)) {
		zunit <- 'unknown'
	}
	if (missing(zname)) {
		zname <- 'value'
	}
	if (missing(varname))  {
		if (nl == 1) {
			varname <- names(x)
		} else if (!is.null(names(x@z))) {
			varname <- names(x@z)
		} else {
			varname <- 'variable'
		}
	}	
	x@title <- varname
	if (missing(varunit))  varunit <- ''
	if (missing(longname))  longname <- varname
	
	if (ncdf4) {
	
		xdim <- ncdf4::ncdim_def( xname, xunit, xFromCol(x, 1:ncol(x)) )
		ydim <- ncdf4::ncdim_def( yname, yunit, yFromRow(x, 1:nrow(x)) )
		if (inherits(x, 'RasterBrick')) {
			zv <- 1:nl
			z <- getZ(x)
			if (!is.null(z)) {
				zv[] <- as.numeric(z)
			}

			zdim <- ncdf4::ncdim_def( zname, zunit, zv, unlim=TRUE )
			vardef <- ncdf4::ncvar_def( varname, varunit, list(xdim,ydim,zdim), NAvalue(x), prec = datatype )
			#vardef <- var.def.ncdf( varname, varunit, list(xdim,ydim,zdim), -3.4e+38 )
		} else {
			#vardef <- var.def.ncdf( varname, varunit, list(xdim,ydim), -3.4e+38 )
			vardef <- ncdf4::ncvar_def( varname, varunit, list(xdim,ydim), NAvalue(x), prec = datatype )
		}
		nc <- ncdf4::nc_create(filename, vardef)

		if (! missing(zatt)){
			for (i in 1:length(zatt)) {
				a <- trim(unlist(strsplit(zatt[i], '=')))
				ncdf4::ncatt_put(nc, zname, a[1], a[2])	
			}
		}

		if (!missing(NAflag)) {
			x@file@nodatavalue <- NAflag
		} 
		
		ncdf4::ncatt_put(nc, varname, '_FillValue', x@file@nodatavalue)
		ncdf4::ncatt_put(nc, varname, 'missing_value', x@file@nodatavalue)
		ncdf4::ncatt_put(nc, varname, 'long_name', longname)

		proj <- projection(x) 
		if (! is.na(proj)) { 
			ncdf4::ncatt_put(nc, varname, 'projection', proj)
			ncdf4::ncatt_put(nc, varname, 'projection_format', 'PROJ.4')
		}

		if (! missing(varatt)){
			for (i in 1:length(varatt)) {
				a <- trim(unlist(strsplit(varatt[i], '=')))
				ncdf4::ncatt_put(nc, varname, a[1], a[2])	
			}
		}

		ncdf4::ncatt_put(nc, 0, 'Conventions', 'CF-1.4')
		if (! missing(att)){
			for (i in 1:length(att)) {
				a <- trim(unlist(strsplit(att[i], '=')))
				ncdf4::ncatt_put(nc, 0, a[1], a[2])	
			}
		}

		
		pkgversion <- drop(read.dcf(file=system.file("DESCRIPTION", package='raster'), fields=c("Version")))
		ncdf4::ncatt_put(nc, 0, 'created_by', paste('R, packages ncdf and raster (version ', pkgversion, ')', sep=''))
		ncdf4::ncatt_put(nc, 0, 'date', format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

		ncdf4::nc_close(nc)
		
	} else {  # library(ncdf)
	
		xdim <- dim.def.ncdf( xname, xunit, xFromCol(x, 1:ncol(x)) )
		ydim <- dim.def.ncdf( yname, yunit, yFromRow(x, 1:nrow(x)) )
		if (inherits(x, 'RasterBrick')) {
			zv <- 1:nl
			z <- getZ(x)
			if (!is.null(z)) {
				zv[] <- as.numeric(z)
			}

			zdim <- dim.def.ncdf( zname, zunit, zv, unlim=TRUE )
			vardef <- var.def.ncdf( varname, varunit, list(xdim,ydim,zdim), NAvalue(x), prec = datatype )
			#vardef <- var.def.ncdf( varname, varunit, list(xdim,ydim,zdim), -3.4e+38 )
		} else {
			#vardef <- var.def.ncdf( varname, varunit, list(xdim,ydim), -3.4e+38 )
			vardef <- var.def.ncdf( varname, varunit, list(xdim,ydim), NAvalue(x), prec = datatype )
		}
		nc <- create.ncdf(filename, vardef)

		if (! missing(zatt)){
			for (i in 1:length(zatt)) {
				a <- trim(unlist(strsplit(zatt[i], '=')))
				att.put.ncdf(nc, zname, a[1], a[2])	
			}
		}

		if (!missing(NAflag)) {
			x@file@nodatavalue <- NAflag
		} 
		
		att.put.ncdf(nc, varname, '_FillValue', x@file@nodatavalue)
		att.put.ncdf(nc, varname, 'missing_value', x@file@nodatavalue)
		att.put.ncdf(nc, varname, 'long_name', longname)

		proj <- projection(x) 
		if (! is.na(proj)) { 
			att.put.ncdf(nc, varname, 'projection', proj)
			att.put.ncdf(nc, varname, 'projection_format', 'PROJ.4')
		}

		if (! missing(varatt)){
			for (i in 1:length(varatt)) {
				a <- trim(unlist(strsplit(varatt[i], '=')))
				att.put.ncdf(nc, varname, a[1], a[2])	
			}
		}

		att.put.ncdf(nc, 0, 'Conventions', 'CF-1.4')
		if (! missing(att)){
			for (i in 1:length(att)) {
				a <- trim(unlist(strsplit(att[i], '=')))
				att.put.ncdf(nc, 0, a[1], a[2])	
			}
		}

		
		pkgversion = drop(read.dcf(file=system.file("DESCRIPTION", package='raster'), fields=c("Version")))
		att.put.ncdf(nc, 0, 'created_by', paste('R, packages ncdf and raster (version ', pkgversion, ')', sep=''))
		att.put.ncdf(nc, 0, 'date', format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

		close.ncdf(nc)
	}
	
	x@data@min <- rep(Inf, nl)
	x@data@max <- rep(-Inf, nl)
	x@data@haveminmax <- FALSE
	x@file@driver <- 'netcdf'
	x@file@name <- filename
	
	return(x)
}


.stopWriteCDF <-  function(x) {
	if (getOption('rasterNCDF4')) {
		nc <- ncdf4::nc_open(x@file@name, write=TRUE)
		on.exit( ncdf4::nc_close(nc) )
		ncdf4::ncatt_put(nc, x@title, 'min', as.numeric(x@data@min))
		ncdf4::ncatt_put(nc, x@title, 'max', as.numeric(x@data@max))
	} else {
		nc <- open.ncdf(x@file@name, write=TRUE)
		on.exit( close.ncdf(nc) )
		att.put.ncdf(nc, x@title, 'min', as.numeric(x@data@min))
		att.put.ncdf(nc, x@title, 'max', as.numeric(x@data@max))
	}

	if (inherits(x, 'RasterBrick')) {
		r <- brick(x@file@name)
	} else {
		r <- raster(x@file@name)
	}
	
	return(r)
}


.writeValuesCDF <- function(x, v, start=1) {

	rsd <- na.omit(v) 
	if (length(rsd) > 0) {
		x@data@min <- min(x@data@min, rsd)
		x@data@max <- max(x@data@max, rsd)
	}	
	
	v[is.na(v)] <- x@file@nodatavalue
	nr <- length(v) / x@ncols
	v <- matrix(v, ncol=nr)

	if (getOption('rasterNCDF4')) {
		nc <- ncdf4::nc_open(x@file@name, write=TRUE)
		on.exit( ncdf4::nc_close(nc) )
		try ( ncdf4::ncvar_put(nc, x@title, v, start=c(1, start), count=c(x@ncols, nr)) )
	} else {
		nc <- open.ncdf(x@file@name, write=TRUE)
		try ( put.var.ncdf(nc, x@title, v, start=c(1, start), count=c(x@ncols, nr)) )
		close.ncdf(nc)
	}
	return(x)
}



.writeValuesBrickCDF <- function(x, v, start=1, layer) {

	if (missing(layer)) { 
		nl <- nlayers(x)
		lstart <- 1
		lend <- nl

		w <- getOption('warn')
		options('warn'=-1) 
		rsd <- apply(v, 2, range, na.rm=TRUE)
		x@data@min <- pmin(x@data@min, rsd[1,])
		x@data@max <- pmax(x@data@max, rsd[2,])
		options('warn'= w) 		

	} else { 
		nl <- 1
		lstart <- layer
		lend <- layer	

		rsd <- na.omit(v) 
		if (length(rsd) > 0) {
			x@data@min[layer] <- min(x@data@min[layer], rsd)
			x@data@max[layer] <- max(x@data@max[layer], rsd)
		}			

	}
	ncols <- x@ncols


	v[is.na(v)] = x@file@nodatavalue
	rows <- length(v) / (ncols * nl)
	v <- array(v, c(rows, ncols, nl))

	if (getOption('rasterNCDF4')) {
		nc <- ncdf4::nc_open(x@file@name, write=TRUE)
		on.exit( ncdf4::nc_close(nc) )
		try ( ncdf4::ncvar_put(nc, x@title, v, start=c(1, start, lstart), count=c(ncols, rows, lend) ) )
	} else {
		nc <- open.ncdf(x@file@name, write=TRUE)
		try ( put.var.ncdf(nc, x@title, v, start=c(1, start, lstart), count=c(ncols, rows, lend) ) )
		close.ncdf(nc)
	}
	
	return(x)
}



#.rasterSaveAsNetCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE, ...) {
#	x <- .startWriteCDF(x, filename=filename, datatype=datatype, overwrite=overwrite, ...)
#	if (nlayers(x) > 1) {
#		x <- .writeValuesBrickCDF(x, getValues(x) )	
#	} else {
#		x <- .writeValuesCDF(x, getValues(x))
#	}
#	return( .stopWriteCDF(x) )
#}



#library(raster)
#r = raster(ncol=10, nrow=5)
#r[] = c(1:49, NA)
#names(r) = 'hello world'
#a = .rasterSaveAsNetCDF(r, 'test.nc', overwrite=TRUE)
#plot(a)
#print(a)


