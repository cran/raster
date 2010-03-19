# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

# based on  create2GDAL and saveDataset from the rgdal package
# authors: Timothy H. Keitt, Roger Bivand, Edzer Pebesma, Barry Rowlingson


.getGDALtransient <- function(raster, filename, options, ...)  {
	r = raster(raster)
	datatype <- .datatype(...)
	overwrite <- .overwrite(...)
	gdalfiletype <- .filetype(...)

	.isSupportedFormat(gdalfiletype)
	
	if (filename == "") {	
		stop('provide a filename')	
	}

	if (file.exists( filename))  {
		if (!overwrite) {
			stop("filename exists; use overwrite=TRUE")
		} else if (!file.remove( filename)) {
			stop("cannot delete existing file. permission denied.")
		}
	}	

	dataformat <- .getGdalDType(datatype, gdalfiletype)
	
    nbands = nlayers(raster)
	if (gdalfiletype=='GTiff') {
		bytes <- ncell(r) * dataSize(datatype) * nbands
		if (bytes > (4 * 1024 * 1024 * 1000) ) {  # ~ 4GB
			options <- c(options, 'BIGTIFF=YES')
		}
	}
	driver = new("GDALDriver", gdalfiletype)
    transient = new("GDALTransientDataset", driver=driver, rows=r@nrows, cols=r@ncols, bands=nbands, type=dataformat, fname=filename, options=options, handle=NULL)
 
	gt <- c(xmin(r), xres(r), 0, ymax(r), 0, -yres(r))
    .Call("RGDAL_SetGeoTransform", transient, gt, PACKAGE = "rgdal")
    p4s <- projection(r)
    .Call("RGDAL_SetProject", transient, p4s, PACKAGE = "rgdal")
	
	return(transient)
}


# ALTERNATIVE; not used
#.spWriteGDALall <- function(raster, gdalfiletype, overwrite, mvFlag, options) {
#	spgrid <- asSpGrid(raster)	
#	writeGDAL(spgrid, filename(raster))
#}


.writeGDALall <- function(raster, filename, options=NULL, ...) {
	if (!require(rgdal)) { stop() }

	mvFlag <- NA
	transient <- .getGDALtransient(raster, filename=filename, mvFlag=mvFlag, options=options, ...)
	nl <- nlayers(raster)
	if (nl == 1) {
		x <- putRasterData(transient, t(values(raster, format='matrix')), band=1, c(0, 0)) 
	} else {
	    for (i in 1:nl) {
			v <- matrix(values(raster)[,i], nrow=nrow(raster), ncol=ncol(raster), byrow=TRUE)
			x <- putRasterData(transient, t(v), band=i, c(0, 0)) 
		}
	}	
	saveDataset(transient, filename )
	GDAL.close(transient) 
	.writeStx(raster) 

	if (nl==1) {
		raster <- raster(filename)
	} else {
		raster <- brick(filename)
	}
	raster <- readAll(raster)
	return(raster)
}

