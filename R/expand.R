# raster package
# Authors: Robert J. Hijmans,  r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("expand")) {
	setGeneric("expand", function(x, y, ...)
		standardGeneric("expand"))
}	


setMethod('expand', signature(x='Raster', y='ANY'), 
function(x, y, filename='', value=NA, ...) {

	if (is.vector(y)) {
		if (length(y) <= 2) {
			adj <- abs(y) * rev(res(x))
			y <- extent(x)
			y@ymin <- y@ymin - adj[1]
			y@ymax <- y@ymax + adj[1]
			y@xmin <- y@xmin - adj[2]
			y@xmax <- y@xmax + adj[2]
		}
	}
	
	test <- try ( y <- extent(y), silent=TRUE )
	if (class(test) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

	filename <- trim(filename)
	
	y  <- alignExtent(y, x)
# only expanding here, not cropping
	y <- unionExtent(y, extent(x))

	if (inherits(x, 'RasterLayer')) {
		outRaster <- raster(x)
	} else {
		outRaster <- brick(x, values=FALSE)
	}
	outRaster@layernames <- layerNames(x)

	outRaster <- setExtent(outRaster, y, keepres=TRUE)
	
	if (! hasValues(x) ) {
		return(outRaster)
	}
	
	datatype <- list(...)$datatype
	if (is.null(datatype)) {
		if (inherits(x, 'RasterStack')) {
			datatype <- 'FLT4S'
		} else {
			datatype <- dataType(x)
		}
	} 
	
	if (canProcessInMemory(outRaster)) {
		d <- matrix(nrow=ncell(outRaster), ncol=nlayers(x))
		d[] <- value
		cells <- cellsFromExtent(outRaster, extent(x))
		d[cells, ] <- getValues(x)
		outRaster <- setValues(outRaster, d)	
		if (filename != '') {
			outRaster <- writeRaster(outRaster, filename=filename, datatype=datatype, ...)
		}
	} else { 
		if (filename == '') {
			filename <- rasterTmpFile()						
		}

		startrow <- rowFromY(outRaster, yFromRow(x, 1))
		endrow <- rowFromY(outRaster, yFromRow(x, nrow(x)))
		startcol <- colFromX(outRaster, xFromCol(x, 1))
		endcol <- colFromX(outRaster, xFromCol(x, ncol(x)))
		
		d <- matrix(nrow=ncol(outRaster), ncol=nlayers(x))
		xr <- 0
		outRaster <- writeStart(outRaster, filename=filename, datatype=datatype, ... )
		pb <- pbCreate(nrow(outRaster), type=.progress(...))
		for (r in 1:nrow(outRaster)) {
			d[] <- value
			if (r >= startrow & r <= endrow) {
				xr <- xr + 1
				d[startcol:endcol, ] <- getValues(x, xr)
			}
			outRaster <- writeValues(outRaster, d, r)
			pbStep(pb, r) 			
		}
		pbClose(pb)
		outRaster <- writeStop(outRaster)
	} 
	return(outRaster)
}
)


