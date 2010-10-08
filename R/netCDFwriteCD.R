# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.rasterSaveAsNetCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE, convention='CF') {

	if (convention=='RST') {
		x <- .startWriteCDFrst(x, filename=filename, datatype=datatype, overwrite=overwrite)
		if (inherits(x, 'RasterBrick')) {
			x <- .writeValuesBrickCDFrst(x, getValues(x) )	
		} else {
			x <- .writeValuesCDFrst(x, getValues(x))
		}
	} else {
		x <- .startWriteCDF(x, filename=filename, datatype=datatype, overwrite=overwrite)
		if (inherits(x, 'RasterBrick')) {
			x <- .writeValuesBrickCDF(x, getValues(x) )	
		} else {
			x <- .writeValuesCDF(x, getValues(x))
		}
	}

	return( .stopWriteCDF(x) )
}


.startWriteCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE, ...) {

	if (!require(ncdf)) { stop('You need to install the ncdf package') }

	filename = trim(filename)
	if (filename == '') { stop('provide a filename') }
	ext(filename) <- .defaultExtension(format='CDF')
	if (file.exists(filename) & !overwrite) {
		stop('file exists, use overwrite=TRUE to overwrite it')
	}
	
	dataType(x) <- datatype
	
	datatype = .getNetCDFDType(datatype)
	
	if (.couldBeLonLat(x)) {
		xname = 'longitude'
		yname = 'latitude'
		unit = 'degrees'
	} else {
		xname = 'northing'
		yname = 'easting'	
		unit = 'meter' # probably
	}
	
	
	xdim <- dim.def.ncdf( xname, unit,  xFromCol(x, 1:ncol(x)) )
	ydim <- dim.def.ncdf( yname, unit, yFromRow(x, 1:nrow(x)) )
	if (inherits(x, 'RasterBrick')) {
		zdim <- dim.def.ncdf( 'z', 'unit', 1:nlayers(x), unlim=TRUE )
		vardef <- var.def.ncdf( 'value', 'unit', list(xdim,ydim,zdim), -3.4e+38 )
	} else {
		vardef <- var.def.ncdf( 'value', 'unit', list(xdim,ydim), -3.4e+38 )
	}
	nc <- create.ncdf(filename, vardef)
	
	att.put.ncdf(nc, 'value', '_FillValue', x@file@nodatavalue)
	att.put.ncdf(nc, 'value', 'missing_value', x@file@nodatavalue)
	att.put.ncdf(nc, 'value', 'long_name', layerNames(x))
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
	
	att.put.ncdf(nc, 'value', 'min', as.numeric(x@data@min))
	att.put.ncdf(nc, 'value', 'max', as.numeric(x@data@max))

	if (inherits(x, 'RasterBrick')) {
		r <- brick(x@file@name, zvar='value')
	} else {
		r <- raster(x@file@name, zvar='value')
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
	try ( put.var.ncdf(nc, 'value', v, start=c(1, start), count=c(x@ncols, nr)) )
	
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
	try ( put.var.ncdf(nc, 'value', v, start=c(1, start, lstart), count=c(ncols, rows, lend) ) )
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

