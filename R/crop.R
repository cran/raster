# Authors: Robert J. Hijmans and Jacob van Etten
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("crop")) {
	setGeneric("crop", function(x, y, ...)
		standardGeneric("crop"))
}	


setMethod('crop', signature(x='RasterStack', y='ANY'), 
function(x, y, filename='', ...) {
	
	if (nlayers(x) == 0) {
		return(stack(crop(raster(x), y, filename='')))
	}

	for (i in 1:nlayers(x)) {
		x@layers[[i]] <- crop(x@layers[[i]], y=y, filename='', ...)
	}
	x@extent <- x@layers[[1]]@extent
	x@nrows <- x@layers[[1]]@nrows
	x@ncols <- x@layers[[1]]@ncols
	x@filename <- ''
	return(x)
	
}
)



setMethod('crop', signature(x='Raster', y='ANY'), 
function(x, y, filename='', datatype=dataType(x), ...) {
	filename <- trim(filename)

	y <- try ( extent(y), silent=TRUE )
	if (class(y) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

# we could also allow the raster to expand but for now let's not and first make a separate expand function
	e <- intersectExtent(x, y)
	e <- alignExtent(e, x)
	
	if (inherits(x, 'RasterBrick')) {
		outRaster <- brick(x, values=FALSE)	
	} else {
		outRaster <- raster(x)
	}
	outRaster <- setExtent(outRaster, e, keepres=TRUE)
	
	if (! inMemory(x)  &  ! fromDisk(x) ) {
		return(outRaster)
	}
	
	col1 <- colFromX(x, xmin(outRaster)+0.5*xres(outRaster))
	col2 <- colFromX(x, xmax(outRaster)-0.5*xres(outRaster))
	row1 <- rowFromY(x, ymax(outRaster)-0.5*yres(outRaster))
	row2 <- rowFromY(x, ymin(outRaster)+0.5*yres(outRaster))
	nc <- ncol(outRaster)
	nr <- row2 - row1 + 1
	
	datatype=dataType(x)

	
	if (canProcessInMemory(outRaster, 3)) {
		x <- getValuesBlock(x, row1, nrows=nr, col=col1, ncols=nc)
		outRaster <- setValues(outRaster, x)
		if (filename != "") { 
			outRaster <- writeRaster(outRaster, filename=filename, datatype=datatype, ...)
		}
	} else { 
		if ( filename == '') {
			filename <- rasterTmpFile()								
		}
		tr <- blockSize(outRaster)
		pb <- pbCreate(tr$n, type=.progress(...))
		outRaster <- writeStart(outRaster, filename=filename, datatype=datatype, ... )
		tr$row <- tr$row
		for (i in 1:tr$n) {
			vv <- getValuesBlock(x, row=tr$row[i]+row1-1, nrows=tr$nrows[i], col1, nc)
			outRaster <- writeValues(outRaster, vv, tr$row[i])
			pbStep(pb, r) 			
		} 
		outRaster <- writeStop(outRaster)
		pbClose(pb)
	}
	return(outRaster)
}
)

