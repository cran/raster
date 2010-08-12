# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.startWriteCDFrst <- function(x, filename, datatype='FLT4S', overwrite=FALSE) {

	if (!require(ncdf)) { stop('You need to install the ncdf package') }

	filename = trim(filename)
	if (filename == '') { stop('provide a filename') }
	ext(filename) <- .defaultExtension(format='CDF')
	if (file.exists(filename) & !overwrite) {
		stop('file exists, use overwrite=TRUE to overwrite it')
	}
	
	dataType(x) <- datatype
	datatype = .getNetCDFDType(datatype)
	

	cdim <- dim.def.ncdf( 'cell', 'cell number',  1:ncell(x) )
	if (inherits(x, 'RasterBrick')) {
		zdim <- dim.def.ncdf( 'z', 'bands', 1:nlayers(x), unlim=TRUE )
		vardef <- var.def.ncdf( 'value', 'unit', list(cdim, zdim) -3.4e+38 )
	} else {
		vardef <- var.def.ncdf( 'value', 'unit', list(cdim), -3.4e+38 )
	}
	nc <- create.ncdf(filename, vardef)

	att.put.ncdf(nc, 0, 'Conventions', 'NC_CHAR', 'RST-0.1')
	pkgversion = drop(read.dcf(file=system.file("DESCRIPTION", package='raster'), fields=c("Version")))
	att.put.ncdf(nc, 0, 'created_by', paste('R, raster package, version', pkgversion))
	att.put.ncdf(nc, 0, 'date', format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

	att.put.ncdf(nc, 0, 'ncols', as.numeric(x@ncols))
	att.put.ncdf(nc, 0, 'nrows', as.numeric(x@nrows))
	att.put.ncdf(nc, 0, 'xmin', x@extent@xmin)
	att.put.ncdf(nc, 0, 'xmax', x@extent@xmax)
	att.put.ncdf(nc, 0, 'ymin', x@extent@ymin)
	att.put.ncdf(nc, 0, 'ymax', x@extent@ymax)
	att.put.ncdf(nc, 0, 'crs', projection(x))
	att.put.ncdf(nc, 'value', 'missing_value', x@file@nodatavalue)
	att.put.ncdf(nc, 'value', 'long_name', layerNames(x))

	close.ncdf(nc)
	
	x@data@min <- rep(Inf, nlayers(x))
	x@data@max <- rep(-Inf, nlayers(x))
	x@data@haveminmax <- FALSE
	x@file@driver <- 'rasterCDF'
	x@file@name <- filename
	
	return(x)
}



.writeValuesCDFrst <- function(x, v, start=1) {

	rsd <- na.omit(v) 
	if (length(rsd) > 0) {
		x@data@min <- min(x@data@min, rsd)
		x@data@max <- max(x@data@max, rsd)
	}	
	
	v[is.na(v)] = x@file@nodatavalue
	v <- matrix(v)

	nc <- open.ncdf(x@file@name, write=TRUE)
	try ( put.var.ncdf(nc, 'value', v, start=c(start, 1), count=NA ) )
	close.ncdf(nc)
	
	return(x)
}



.writeValuesBrickCDFrst <- function(x, v, start=1, layer) {

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
	
	nc <- open.ncdf(x@file@name, write=TRUE)
	try (  put.var.ncdf(nc, 'value', v, start=c(start, lstart), count=c(nrow(v), lend) )  )
	close.ncdf(nc)
	
	return(x)
}


.stopWriteCDFrst <-  function(x) {
	nc <- open.ncdf(x@file@name, write=TRUE)
	att.put.ncdf(nc, 'value', 'min', 'double', as.numeric(x@data@min))
	att.put.ncdf(nc, 'value', 'max', 'double', as.numeric(x@data@max))
	close.ncdf(nc)

	if (inherits(x, 'RasterBrick')) {
		r <- brick(x@file@name, convention='RST')
	} else {
		r <- raster(x@file@name, convention='RST')
	}
	return(r)
}



#library(raster)
#r = raster(ncol=10, nrow=5)
#r[] = c(1:49, NA)
#layerNames(r) = 'hello world'
#a = raster:::.rasterSaveAsNetCDF(r, 'test.nc', overwrite=TRUE)
#plot(a)
#print(a)

