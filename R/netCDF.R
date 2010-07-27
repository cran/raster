# Author: Robert J. Hijmans and Michael Sumner
# Date: June 2010
# Version 1.0
# Licence GPL v3


.readCDF <- function(filename, xvar='', yvar='', varname='', bands, type='RasterLayer', sp=FALSE, ...) {

	if (!require(RNetCDF)) { stop('You need to install the RNetCDF package first') }
	if (missing(filename)) { stop('provide a filename') }
	filename = trim(filename)
	if (filename == '') { stop('provide a filename') }

	if (type == 'RasterLayer') {
		return( .rasterObjectFromCDF(filename, x=xvar, y=vvar, varname=varname, band=bands[1], type='RasterLayer', ...) )
	} else if (type == 'RasterBrick') {
		return( .rasterObjectFromCDF(filename, x=xvar, y=vvar, varname=varname, type='RasterBrick', ...) )
	} else if (type == 'RasterStack') {
		return( .stackCDF(filename, x=xvar, y=yvar, varname=varname, bands=bands, ...) )
	} else {
		nc <- open.nc(filename)
		on.exit( close.nc(nc) )
		nv <- file.inq.nc(nc)$nvars
		vars <- vector()
		for (i in 1:nv) { vars <- c(var.inq.nc(nc,i-1)$name, vars) }
		xvar <- .getxvar(x, vars)
		yvar <- .getyvar(y, vars)
		vvar <- .getVarname(varname, vars) 
		varinfo <- var.inq.nc(nc, vvar)
		ndims <- varinfo$ndims
		if (ndims > 3) {stop()}
		
		x <- as.vector(var.get.nc(nc, xvar))
		y <- as.vector(var.get.nc(nc, yvar))

		if (file.inq.nc(nc)$ndims == 3) {
			nbands <- dim.inq.nc(nc, varinfo$dimids[3])$length
			if (missing(bands)) {
				bands <- 1:nbands
			} else {
				bands <- min(bands):max(bands)
				bands <- subset(bands, bands > 0 & bands <= nbands)
			}
			start = c(1, 1, bands[1])
			count = c(dim.inq.nc(nc, varinfo$dimids[1])$length, dim.inq.nc(nc, varinfo$dimids[2])$length, bands[length(bands)])
			v <- var.get.nc(nc, variable=vvar, start=start, count=count)
		} else {
			v <- var.get.nc(nc, variable=vvar)		
		}
		
		
		if (type == 'points') {
			if (ndims == 1) {
				pts <- cbind(x, y, varname=as.vector(v)) 	
			} else if (ndims == 2) {
				y <- rep(y, times=dim(v)[1])
				x <- rep(x, each=dim(v)[2])
				pts <- cbind(x, y, varname=as.vector(v)) 	
			} else {
				y <- rep(y, times=dim(v)[1])
				x <- rep(x, each=dim(v)[2])
				#dim(v) <- c()
				pts <- cbind(x, y, varname=as.vector(v)) 	
			}
			
			if (sp) {
				pts <- data.frame(pts)
				coordinates(pts) <- ~ x + y
			}
			return(pts)
			
		} else {
		
		
		}
	
	
	}
}

