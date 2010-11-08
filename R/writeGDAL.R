# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

# based on  create2GDAL and saveDataset from the rgdal package
# authors: Timothy H. Keitt, Roger Bivand, Edzer Pebesma, Barry Rowlingson

..xxnodatavalue <- function(x){
	if (x == 'FLT4S') return(-3.4E38)
	if (x == 'FLT8S') return(-1.7E308)
	if (x == 'INT4S') return(-2147483647)
    if (x == 'INT2S') return(-32768)
    if (x == 'INT2U') return(65535)
    if (x == 'INT1U') return(-1)
    if (x == 'INT1S') return(-127)
	if (x == 'INT8S') return(-9223372036854775808)
	stop('cannot find matching nodata value')
}

.GDALnodatavalue <- function(x){
	if (x == 'Float32') return(-3.4E38)
	if (x == 'Float64') return(-1.7E308)
	if (x == 'Int32') return(-2147483647)
    if (x == 'Int16') return(-32768)
    if (x == 'Int8') return(-128)
    if (x == 'UInt16') return(65535)
    if (x == 'Byte') return(-1)
	stop('cannot find matching nodata value')
}

.getGDALtransient <- function(r, filename, options, NAvalue, ...)  {
    nbands <- nlayers(r)
	r <- raster(r)
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
	
	if (missing(NAvalue)) { 
		NAvalue <- .GDALnodatavalue(dataformat) 
	}
	
	if (gdalfiletype=='GTiff') {
		bytes <- ncell(r) * dataSize(datatype) * nbands
		if (bytes > (4 * 1024 * 1024 * 1000) ) {  # ~ 4GB
			options <- c(options, 'BIGTIFF=YES')
		}
		options <- c(options, "COMPRESS=LZW")
	}

	driver = new("GDALDriver", gdalfiletype)
	
    transient = new("GDALTransientDataset", driver=driver, rows=r@nrows, cols=r@ncols, bands=nbands, type=dataformat, fname=filename, options=options, handle=NULL)
 
	for (i in 1:nbands) {
		b <- new("GDALRasterBand", transient, i)
		.Call("RGDAL_SetNoDataValue", b, as.double(NAvalue), PACKAGE = "rgdal")
	}

	gt <- c(xmin(r), xres(r), 0, ymax(r), 0, -yres(r))
    .Call("RGDAL_SetGeoTransform", transient, gt, PACKAGE = "rgdal")
    .Call("RGDAL_SetProject", transient, projection(r), PACKAGE = "rgdal")

	if (is.null(options)) options <- ''
	return(list(transient, NAvalue, options))
}


.writeGDALall <- function(raster, filename, options=NULL, ...) {

	if (! .requireRgdal() ) { stop('rgdal not available') }

	raster <- .startGDALwriting(raster, filename, options, NAvalue, ...) 
	
	nl <- nlayers(raster)
	if (nl == 1) {
		v <- as.matrix(raster)
		v[is.na(v)] <- raster@file@nodatavalue
		x <- putRasterData(raster@file@transient, t(v), band=1, c(0, 0)) 
	} else {
	    for (i in 1:nl) {
			v <- getValues(raster)[,i]
			v[is.na(v)] = raster@file@nodatavalue
			v <- matrix(v, nrow=raster@nrows, ncol=raster@ncols, byrow=TRUE)
			x <- putRasterData(raster@file@transient, t(v), band=i, c(0, 0))
		}
	}	
	
	return( .stopGDALwriting(raster) )
}

