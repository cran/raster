# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric('writeRaster')) {
	setGeneric('writeRaster', function(x, filename, ...)
		standardGeneric('writeRaster')) 
}


setMethod('writeRaster', signature(x='RasterLayer', filename='character'), 
function(x, filename, format, ...) {

	filename <- trim(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	
	dc <- dataContent(x)
	if (dc == 'nodata') {
		if (dataSource(x) == 'disk') {
			return( saveAs(x, filename, format=filetype, ...) )
		} else {
			stop('No usable data available for writing.')
		}
	}
	
	
	if (.isNativeDriver(filetype)) {
		if (substr(dc, 1, 3) == 'row' ) {
			x <- .writeRasterRow(x, filename=filename, format=filetype, ...)
		} else {
			x <- .writeRasterAll(x, filename=filename, format=filetype, ...)
		}  
	} else if (filetype=='ascii') {
		x <- .writeAscii(x, filename=filename, format=filetype,...)
	} else if (filetype=='CDF') {
		x <- .writeRasterCDF(x, filename=filename, format=filetype, ...)
	} else { 
		if (substr(dc, 1, 3) == 'row' ) {
			x <- .writeGDALrow(x, filename=filename, format=filetype, ...)
		} else if (dc == 'all') {
			x <- .writeGDALall(x, filename=filename, format=filetype, ...)
		} else {
			stop('cannot write data')
		}		
	}
	return(x)
}	
)


setMethod('writeRaster', signature(x='RasterBrick', filename='character'), 
function(x, filename, bandorder='BIL', format, ...) {

	filename <- trim(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)
	
	dc <- dataContent(x)
	if (! dc %in% c('row', 'all') ) {
		if (dataSource(x) == 'disk') {
			return( saveAs(x, filename, bandorder=bandorder, format=filetype, ...) )
		} else {
			stop('No usable data available for writing.')
		}
	}

	if (filetype=='raster') {
		if (dc == 'row' ) {
			return( .writeBrickRow(object=x, filename=filename, bandorder=bandorder, ...) )
		} else {
			return( .writeBrick(object=x, filename=filename, format=filetype, bandorder=bandorder, ...) )
		}
	} else {
		if (dc == 'row' ) {
			x <- .writeGDALrow(x, filename=filename, format=filetype, ...)
		} else {
			x <- .writeGDALall(x, filename=filename, format=filetype, ...)
		}
	}
}
)


setMethod('writeRaster', signature(x='RasterStack', filename='character'), 
function(x, filename, bandorder='BIL', format, ...) {

	filename <- trim(filename)
	filetype <- .filetype(format=format, filename=filename)
	filename <- .getExtension(filename, filetype)

	b <- brick(x, values=FALSE)
	b <- writeStart(b, filename=filename, bandorder=bandorder, format=filetype, ...)
	tr <- blockSize(b)
	pb <- pbCreate(tr$n, type=.progress(...))
	for (i in 1:tr$n) {
		v <- getValuesBlock(x, row=tr$row[i], nrows=tr$size)
		writeValues(b, v, tr$row[i])
		pbStep(pb, i)
	}
	pbClose(pb)
	b <- writeStop(b)
	return(invisible(b))
	
}
)

