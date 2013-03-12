# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3

# based on  create2GDAL and saveDataset from the rgdal package
# authors: Timothy H. Keitt, Roger Bivand, Edzer Pebesma, Barry Rowlingson



.getGDALtransient <- function(r, filename, options, NAflag, ...)  {

	.GDALnodatavalue <- function(x){
		if (x == 'Float32') return(-3.4E38)
		if (x == 'Float64') return(-1.7E308)
		if (x == 'Int32') return(-2147483647)
		if (x == 'Int16') return(-32768)
		if (x == 'Int8') return(-128)
		if (x == 'Byte') return(255)
		if (x == 'UInt16') return(65535)
		if (x == 'UInt32') return(2147483647) #(4294967295) <- not supported as integer in R
		stop('cannot find matching nodata value')
	}


    nbands <- nlayers(r)
	ct <- r@legend@colortable
	if (length(ct) > 0 ) {
		hasCT <- TRUE
	} else {
		hasCT <- FALSE
	}
	r <- raster(r)
	datatype <- .datatype(...)
	overwrite <- .overwrite(...)
	gdalfiletype <- .filetype(filename=filename, ...)

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
	
	if (dataformat != 'Byte') hasCT <- FALSE
		
	if (missing(NAflag)) { 
		NAflag <- .GDALnodatavalue(dataformat) 
	}
	
	if (gdalfiletype=='GTiff') {
		bytes <- ncell(r) * dataSize(datatype) * nbands
		if (bytes > (4 * 1024 * 1024 * 1000) ) {  # ~ 4GB
			options <- c(options, 'BIGTIFF=YES')
		}
		options <- c(options, "COMPRESS=LZW")
	}

	driver <- new("GDALDriver", gdalfiletype)
	
    transient <- new("GDALTransientDataset", driver=driver, rows=r@nrows, cols=r@ncols, bands=nbands, type=dataformat, fname=filename, options=options, handle=NULL)
 
	for (i in 1:nbands) {
		b <- new("GDALRasterBand", transient, i)
		rgdal:::.gd_SetNoDataValue(b, NAflag)
		if (hasCT) {
			try( .SetRasterColorTable. <- rgdal:::.gd_SetRasterColorTable, silent=TRUE)
			if (exists(".SetRasterColorTable.")) {
				rgdal:::.gd_SetRasterColorTable(b, t(col2rgb(ct, TRUE)))
			}
		}
	}

	if (rotated(r)) {
		gt <- r@rotation@geotrans
	} else {
		#if (flip) {
		#	gt <- c(xmin(r), xres(r), 0, 0, ymax(r), yres(r))		
		#	cat('flipping (this creates an invalid RasterLayer)\n')
		#} else {
		gt <- c(xmin(r), xres(r), 0, ymax(r), 0, -yres(r))
		#}
	}
	
	rgdal:::.gd_SetGeoTransform(transient, gt)
	rgdal:::.gd_SetProject(transient, projection(r))
		
	if (is.null(options)) {
		options <- ''
	}
	return(list(transient, NAflag, options, dataformat))
}
