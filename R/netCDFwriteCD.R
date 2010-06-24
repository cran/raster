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


.startWriteCDF <- function(x, filename, datatype='FLT4S', overwrite=FALSE) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package') }

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
	
	nc <- create.nc(filename)	
	
	dim.def.nc(nc, 'x', ncol(x) )
	var.def.nc(nc, 'x', "NC_DOUBLE", 0)
	att.put.nc(nc, 'x', 'long_name', 'NC_CHAR', xname)
	att.put.nc(nc, 'x', 'standard_name', 'NC_CHAR', xname)
	att.put.nc(nc, 'x', 'axis', 'NC_CHAR', 'X')
	att.put.nc(nc, 'x', 'units', 'NC_CHAR', unit)

	
	dim.def.nc(nc, 'y', nrow(x) ) 
	var.def.nc(nc, 'y', "NC_DOUBLE", 1)
	att.put.nc(nc, 'y', 'long_name', 'NC_CHAR', yname)
	att.put.nc(nc, 'y', 'standard_name', 'NC_CHAR', yname)
	att.put.nc(nc, 'y', 'axis', 'NC_CHAR', 'Y')
	att.put.nc(nc, 'y', 'units', 'NC_CHAR', unit)

	var.put.nc(nc, 'x', xFromCol(x, 1:ncol(x)), start=NA, count=NA, na.mode=0)
	var.put.nc(nc, 'y', yFromRow(x, 1:nrow(x)), start=NA, count=NA, na.mode=0)

	if (inherits(x, 'RasterBrick')) {
		dim.def.nc(nc, 'z', nlayers(x), unlim=TRUE)
		var.def.nc(nc, 'z', "NC_INT", 2)
		var.put.nc(nc, 'z', 1:nlayers(x), start=NA, count=NA, na.mode=0)
		var.def.nc(nc, 'value', datatype, c(0,1,2))
	} else {
		var.def.nc(nc, 'value', datatype, c(0,1))	
	}
	
	att.put.nc(nc, 'value', 'missing_value', datatype, x@file@nodatavalue)
	att.put.nc(nc, 'value', 'long_name', 'NC_CHAR', layerNames(x))

	att.put.nc(nc, "NC_GLOBAL", 'Conventions', 'NC_CHAR', 'CF-1.4')
	pkgversion = drop(read.dcf(file=system.file("DESCRIPTION", package='raster'), fields=c("Version")))
	att.put.nc(nc, "NC_GLOBAL", 'created_by', 'NC_CHAR', paste('R, raster package, version', pkgversion))
	att.put.nc(nc, "NC_GLOBAL", 'date', 'NC_CHAR', format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

	close.nc(nc)
	
	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))
	x@data@haveminmax <- FALSE
	x@file@driver <- 'netcdf'
	x@file@name <- filename
	
	return(x)
}


.stopWriteCDF <-  function(x) {
	nc <- open.nc(x@file@name, write=TRUE)
	att.put.nc(nc, 'value', 'min', 'NC_DOUBLE', as.numeric(x@data@min))
	att.put.nc(nc, 'value', 'max', 'NC_DOUBLE', as.numeric(x@data@max))
	close.nc(nc)

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
	v <- matrix(v, ncol=x@nrows)

	nc <- open.nc(x@file@name, write=TRUE)
	try ( var.put.nc(nc, 'value', v, start=c(1, start), count=NA, na.mode=0) )
	close.nc(nc)
	
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
	
	nc <- open.nc(x@file@name, write=TRUE)
	try ( var.put.nc(nc, 'value', v, start=c(1, start, lstart), count=c(ncols, rows, lend), na.mode=0) )
	close.nc(nc)
	
	return(x)
}



#library(raster)
#r = raster(ncol=10, nrow=5)
#r[] = c(1:49, NA)
#layerNames(r) = 'hello world'
#a = raster:::.rasterSaveAsNetCDF(r, 'test.nc', overwrite=TRUE)
#plot(a)
#print(a)

