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
function(x, y, filename='', datatype, ...) {

	filename <- trim(filename)

	y <- try ( extent(y), silent=TRUE )
	if (class(y) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}
	validObject(y)
	
	if (missing(datatype)) { 
		datatype <- unique(dataType(x))
		if (length(datatype) > 1) {
			datatype <- .commonDataType(datatype)
		}
	}

# we could also allow the raster to expand but for now let's not and first make a separate expand function
	e <- intersectExtent(x, y)
	e <- alignExtent(e, x)
	

	if (nlayers(x) <= 1) {
		out <- raster(x)
		leg <- x@legend
	} else {
		out <- brick(x, values=FALSE)	
		leg <- new('.RasterLegend')
	}

	out <- setExtent(out, e, keepres=TRUE)
	layerNames(out) <- layerNames(x)
	
	if (! hasValues(x)) {
		return(out)
	}
	
	col1 <- colFromX(x, xmin(out)+0.5*xres(out))
	col2 <- colFromX(x, xmax(out)-0.5*xres(out))
	row1 <- rowFromY(x, ymax(out)-0.5*yres(out))
	row2 <- rowFromY(x, ymin(out)+0.5*yres(out))
	if (row1==1 & row2==nrow(x) & col1==1 & col2==ncol(x)) {
		return(x)
	}

	nc <- ncol(out)
	nr <- row2 - row1 + 1
	
	
	if (canProcessInMemory(out, 3)) {
		x <- getValuesBlock(x, row1, nrows=nr, col=col1, ncols=nc)
		out <- setValues(out, x)
		if (filename != "") { 
			out <- writeRaster(out, filename=filename, datatype=datatype, ...)
		}
	} else { 
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, ...)
		out <- writeStart(out, filename=filename, datatype=datatype, ... )
		for (i in 1:tr$n) {
			vv <- getValuesBlock(x, row=tr$row[i]+row1-1, nrows=tr$nrows[i], col1, nc)
			out <- writeValues(out, vv, tr$row[i])
			pbStep(pb, i) 			
		} 
		out <- writeStop(out)
		pbClose(pb)
	}

	out@legend <- leg
	return(out)
}
)

