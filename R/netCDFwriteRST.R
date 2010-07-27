# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


.startWriteCDFrst <- function(x, filename, datatype='FLT4S', overwrite=FALSE) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package') }

	filename = trim(filename)
	if (filename == '') { stop('provide a filename') }
	ext(filename) <- .defaultExtension(format='CDF')
	if (file.exists(filename) & !overwrite) {
		stop('file exists, use overwrite=TRUE to overwrite it')
	}
	
	dataType(x) <- datatype
	datatype = raster:::.getNetCDFDType(datatype)
	
	nc <- create.nc(filename)	

	att.put.nc(nc, "NC_GLOBAL", 'Conventions', 'NC_CHAR', 'RST-0.1')
	
	pkgversion = drop(read.dcf(file=system.file("DESCRIPTION", package='raster'), fields=c("Version")))
	att.put.nc(nc, "NC_GLOBAL", 'created_by', 'NC_CHAR', paste('R, raster package, version', pkgversion))
	att.put.nc(nc, "NC_GLOBAL", 'date', 'NC_CHAR', format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

	att.put.nc(nc, "NC_GLOBAL", 'ncols', 'NC_INT', as.numeric(x@ncols))
	att.put.nc(nc, "NC_GLOBAL", 'nrows', 'NC_INT', as.numeric(x@nrows))
	att.put.nc(nc, "NC_GLOBAL", 'xmin', 'NC_DOUBLE', x@extent@xmin)
	att.put.nc(nc, "NC_GLOBAL", 'xmax', 'NC_DOUBLE', x@extent@xmax)
	att.put.nc(nc, "NC_GLOBAL", 'ymin', 'NC_DOUBLE', x@extent@ymin)
	att.put.nc(nc, "NC_GLOBAL", 'ymax', 'NC_DOUBLE', x@extent@ymax)
	att.put.nc(nc, "NC_GLOBAL", 'crs', 'NC_CHAR', projection(x))
	

	dim.def.nc(nc, 'cell', ncell(x) )
	var.def.nc(nc, 'x', "NC_INT", 0)
	att.put.nc(nc, 'x', 'long_name', 'NC_CHAR', 'cell number')
	att.put.nc(nc, 'x', 'standard_name', 'NC_CHAR', 'cell number')

	dim.def.nc(nc, 'z', nlayers(x), unlim=TRUE)
	var.def.nc(nc, 'z', "NC_INT", 1)
	var.put.nc(nc, 'z', 1:nlayers(x), start=NA, count=NA, na.mode=0)
	var.def.nc(nc, 'value', datatype, c(0,1))
	
	att.put.nc(nc, 'value', 'missing_value', datatype, x@file@nodatavalue)
	att.put.nc(nc, 'value', 'long_name', 'NC_CHAR', layerNames(x))

	close.nc(nc)
	
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

	nc <- open.nc(x@file@name, write=TRUE)
	try ( var.put.nc(nc, 'value', v, start=c(start, 1), count=NA, na.mode=0) )
	close.nc(nc)
	
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
	
	nc <- open.nc(x@file@name, write=TRUE)
	try ( var.put.nc(nc, 'value', v, start=c(start, lstart), count=c(nrow(v), lend), na.mode=0) )
	close.nc(nc)
	
	return(x)
}


.stopWriteCDFrst <-  function(x) {
	nc <- open.nc(x@file@name, write=TRUE)
	att.put.nc(nc, 'value', 'min', 'NC_DOUBLE', as.numeric(x@data@min))
	att.put.nc(nc, 'value', 'max', 'NC_DOUBLE', as.numeric(x@data@max))
	close.nc(nc)

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

