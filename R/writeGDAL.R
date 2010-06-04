# R function for the raster package
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
    if (x == 'INT1S') return(-129)
	if (x == 'INT8S') return(-9223372036854775808)
}

.GDALnodatavalue <- function(x){
	if (x == 'Float32') return(-3.4E38)
	if (x == 'Float64') return(-1.7E308)
	if (x == 'Int32') return(-2147483647)
    if (x == 'Int16') return(-32768)
    if (x == 'UInt16') return(65535)
    if (x == 'Byte') return(-1)
}

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
	naValue <- .GDALnodatavalue(dataformat)
    nbands = nlayers(raster)
	
	if (gdalfiletype=='GTiff') {
		bytes <- ncell(r) * dataSize(datatype) * nbands
		if (bytes > (4 * 1024 * 1024 * 1000) ) {  # ~ 4GB
			options <- c(options, 'BIGTIFF=YES')
		}
	}
	driver = new("GDALDriver", gdalfiletype)
    transient = new("GDALTransientDataset", driver=driver, rows=r@nrows, cols=r@ncols, bands=nbands, type=dataformat, fname=filename, options=options, handle=NULL)
 
	for (i in 1:nbands) {
		b <- new("GDALRasterBand", transient, i)
		.Call("RGDAL_SetNoDataValue", b, as.double(naValue), PACKAGE = "rgdal")
	}

 
	gt <- c(xmin(r), xres(r), 0, ymax(r), 0, -yres(r))
    .Call("RGDAL_SetGeoTransform", transient, gt, PACKAGE = "rgdal")
    p4s <- projection(r)
    .Call("RGDAL_SetProject", transient, p4s, PACKAGE = "rgdal")

	return(list(transient, naValue))
}


# ALTERNATIVE; not used
#.spWriteGDALall <- function(raster, gdalfiletype, overwrite, mvFlag, options) {
#	spgrid <- asSpGrid(raster)	
#	writeGDAL(spgrid, filename(raster))
#}


.writeGDALall <- function(raster, filename, options=NULL, ...) {

	if (! .requireRgdal() ) { stop('rgdal not available') }

	temp <- .getGDALtransient(raster, filename=filename, options=options, ...)
	transient <- temp[[1]]
	naValue <- temp[[2]]
	nl <- nlayers(raster)

	if (nl == 1) {
		v <- getValues(raster, format='matrix')
		v[is.na(v)] = naValue
		x <- putRasterData(transient, t(v), band=1, c(0, 0)) 
	} else {
	    for (i in 1:nl) {
			v <- getValues(raster)[,i]
			v[is.na(v)] = naValue
			v <- matrix(v, nrow=nrow(raster), ncol=ncol(raster), byrow=TRUE)
			x <- putRasterData(transient, t(v), band=i, c(0, 0))
		}
	}	

 	saveDataset(transient, filename )
	GDAL.close(transient) 
#	.writeStx(raster, filename) 

	if (nl==1) {
		raster <- raster(filename)
	} else {
		raster <- brick(filename)
	}
#	raster <- readAll(raster)
	return(raster)
}

