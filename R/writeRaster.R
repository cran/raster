# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, filename, ...)
		standardGeneric('writeRaster')) 
}  
	

setMethod('writeRaster', signature(x='RasterLayer', filename='character'), 
function(x, filename, ...) {

	filetype <- .filetype(...)

	filename <- .getExtension(filename, filetype)
	
	dc <- dataContent(x)
	if (dc == 'nodata') {
		if (dataSource(x) == 'disk') {
			return( saveAs(x, filename, ...) )
		} else {
			stop('No usable data available for writing.')
		}
	}
	
	
	if (.isNativeDriver(filetype)) {
		if (substr(dc, 1, 3) == 'row' ) {
			x <- .writeRasterRow(x, filename=filename, ...)
		} else {
			x <- .writeRasterAll(x, filename=filename, ...)
		}  
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, filename=filename, ...)
	} else if (filetype=='CDF') {
		x <- .writeRasterCDF(x, filename=filename, ...)
	} else { 
		if (substr(dc, 1, 3) == 'row' ) {
			x <- .writeGDALrow(x, filename=filename, ...)
		} else if (dc == 'all') {
			x <- .writeGDALall(x, filename=filename, ...)
		} else {
			stop('cannot write data')
		}		
	}
	return(x)
}	
)


setMethod('writeRaster', signature(x='RasterBrick', filename='character'), 
function(x, filename, bandorder='BIL', ...) {

	filetype <- .filetype(...)
	filename <- .getExtension(filename, filetype)
	
	dc <- dataContent(x)
	if (! dc %in% c('row', 'all') ) {
		if (dataSource(x) == 'disk') {
			return( saveAs(x, filename, bandorder=bandorder, ...) )
		} else {
			stop('No usable data available for writing.')
		}
	}

	if (filetype=='raster') {
		if (dc == 'row' ) {
			return( .writeBrickRow(object=x, filename=filename, bandorder=bandorder, ...) )
		} else {
			return( .writeBrick(object=x, filename=filename, bandorder=bandorder, ...) )
		}
	} else {
		if (dc == 'row' ) {
			x <- .writeGDALrow(x, filename=filename, ...)
		} else {
			x <- .writeGDALall(x, filename=filename, ...)
		}
	}
}
)


setMethod('writeRaster', signature(x='RasterStack', filename='character'), 
function(x, filename, bandorder='BIL', ...) {
	test <- try( b <- brick(x) )
	if (class(test) != "try-error") {
		b <- writeRaster(b, filename=filename, bandorder=bandorder, ...)
		return(invisible(b))
	}

	b <- brick(raster(x))
	for (r in 1:nrow(x)) {
		v <- getValues(x, r)
		b <- setValues(b, v, r)
		b <- writeRaster(b, filename=filename, bandorder=bandorder, ...)
	}
	return(invisible(b))
}
)
