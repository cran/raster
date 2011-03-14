# Authors: Robert J. Hijmans and Jacob van Etten
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3




if (!isGeneric("crop")) {
	setGeneric("crop", function(x, y, ...)
		standardGeneric("crop"))
}	


setMethod('crop', signature(x='Raster', y='ANY'), 
function(x, y, filename='', ...) {
	filename <- trim(filename)
	leg <- x@legend

	datatype <- list(...)$datatype
	if (is.null(datatype)) { 
		datatype <- dataType(x)
		if (length(unique(datatype)) > 1) {
			datatype <- 'FLT4S'
		} else {
			datatype <- datatype[1]
		}
	}
	
	y <- try ( extent(y), silent=TRUE )
	if (class(y) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

# we could also allow the raster to expand but for now let's not and first make a separate expand function
	e <- intersectExtent(x, y)
	e <- alignExtent(e, x)
	
	if (inherits(x, 'RasterLayer')) {
		outRaster <- raster(x)
	} else {
		outRaster <- brick(x, values=FALSE)	
	}
	outRaster <- setExtent(outRaster, e, keepres=TRUE)
	outRaster@layernames <- layerNames(x)
	
	if (! hasValues(x)) {
		return(outRaster)
	}
	
	col1 <- colFromX(x, xmin(outRaster)+0.5*xres(outRaster))
	col2 <- colFromX(x, xmax(outRaster)-0.5*xres(outRaster))
	row1 <- rowFromY(x, ymax(outRaster)-0.5*yres(outRaster))
	row2 <- rowFromY(x, ymin(outRaster)+0.5*yres(outRaster))
	nc <- ncol(outRaster)
	nr <- row2 - row1 + 1
	
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
		for (i in 1:tr$n) {
			vv <- getValuesBlock(x, row=tr$row[i]+row1-1, nrows=tr$nrows[i], col1, nc)
			outRaster <- writeValues(outRaster, vv, tr$row[i])
			pbStep(pb, i) 			
		} 
		outRaster <- writeStop(outRaster)
		pbClose(pb)
	}
	outRaster@legend <- leg
	return(outRaster)
}
)

