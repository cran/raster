# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.rasterSaveAsNetCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE, convention='CF', ...) {

	if (convention=='RST') {
		x <- .startWriteCDFrst(x, filename=filename, datatype=datatype, overwrite=overwrite, ...)
		if (inherits(x, 'RasterBrick')) {
			x <- .writeValuesBrickCDFrst(x, getValues(x) )	
		} else {
			x <- .writeValuesCDFrst(x, getValues(x))
		}
	} else {
		x <- .startWriteCDF(x, filename=filename, datatype=datatype, overwrite=overwrite, ...)
		if (inherits(x, 'RasterBrick')) {
			x <- .writeValuesBrickCDF(x, getValues(x) )	
		} else {
			x <- .writeValuesCDF(x, getValues(x))
		}
	}

	return( .stopWriteCDF(x) )
}


.startWriteCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE, varname, varunit, longname, xname, yname, zname, zunit, ...) {

	if (!require(ncdf)) { stop('You need to install the ncdf package') }

	filename = trim(filename)
	if (filename == '') { stop('provide a filename') }
	ext(filename) <- raster:::.defaultExtension(format='CDF')
	if (file.exists(filename) & !overwrite) {
		stop('file exists, use overwrite=TRUE to overwrite it')
	}
	
	dataType(x) <- datatype
	
	datatype = raster:::.getNetCDFDType(datatype)
	
	if (.couldBeLonLat(x)) {
		if (missing(xname)) xname = 'longitude'
		if (missing(yname)) yname = 'latitude'
		xunit = 'degrees_east'
		yunit = 'degrees_north'
	} else {
		if (missing(xname)) xname = 'northing'
		if (missing(yname)) yname = 'easting'	
		xunit = 'meter' # probably
		yunit = 'meter' # probably
	}
	
	
	if (missing(zunit))  zunit <- 'unknown'
	if (missing(zname))  zname <- 'value'
	x@zname <- zname
	if (missing(varname))  varname <- 'variable'
	x@title <- varname
	if (missing(varunit))  varunit <- ''
	if (missing(longname))  longname <- varname
	
	
	xdim <- dim.def.ncdf( xname, xunit, xFromCol(x, 1:ncol(x)) )
	ydim <- dim.def.ncdf( yname, yunit, yFromRow(x, 1:nrow(x)) )
	if (inherits(x, 'RasterBrick')) {
		zv <- 1:nlayers(x)
		zv[] <- as.numeric(x@zvalue)
		if (any(is.na(zv))) {
			zv <- 1:nlayers(x)
		}
		zdim <- dim.def.ncdf( zname, zunit, zv, unlim=TRUE )
		vardef <- var.def.ncdf( varname, varunit, list(xdim,ydim,zdim), -3.4e+38 )
	} else {
		vardef <- var.def.ncdf( varname, varunit, list(xdim,ydim), -3.4e+38 )
	}
	nc <- create.ncdf(filename, vardef)
	
	att.put.ncdf(nc, varname, '_FillValue', x@file@nodatavalue)
	att.put.ncdf(nc, varname, 'missing_value', x@file@nodatavalue)
	att.put.ncdf(nc, varname, 'long_name', longname)
	att.put.ncdf(nc, 0, 'Conventions', 'CF-1.4')
	pkgversion = drop(read.dcf(file=system.file("DESCRIPTION", package='raster'), fields=c("Version")))
	att.put.ncdf(nc, 0, 'created_by', paste('R, raster package, version', pkgversion))
	att.put.ncdf(nc, 0, 'date', format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
	close.ncdf(nc)
	
	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))
	x@data@haveminmax <- FALSE
	x@file@driver <- 'netcdf'
	x@file@name <- filename
	
	return(x)
}


.stopWriteCDF <-  function(x) {
	nc <- open.ncdf(x@file@name, write=TRUE)
	on.exit( close.ncdf(nc) )
	
	att.put.ncdf(nc, x@title, 'min', as.numeric(x@data@min))
	att.put.ncdf(nc, x@title, 'max', as.numeric(x@data@max))

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
	
	v[is.na(v)] = x@file@nodatavalue
	nr <- length(v) / x@ncols
	v <- matrix(v, ncol=nr)

	nc <- open.ncdf(x@file@name, write=TRUE)
	try ( put.var.ncdf(nc, x@title, v, start=c(1, start), count=c(x@ncols, nr)) )
	
	close.ncdf(nc)
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
	
	nc <- open.ncdf(x@file@name, write=TRUE)
	try ( put.var.ncdf(nc, x@title, v, start=c(1, start, lstart), count=c(ncols, rows, lend) ) )
	close.ncdf(nc)
	
	return(x)
}



#library(raster)
#r = raster(ncol=10, nrow=5)
#r[] = c(1:49, NA)
#layerNames(r) = 'hello world'
#a = raster:::.rasterSaveAsNetCDF(r, 'test.nc', overwrite=TRUE)
#plot(a)
#print(a)

