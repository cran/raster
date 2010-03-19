# raster package
# Authors: Robert J. Hijmans,  r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("expand")) {
	setGeneric("expand", function(x, y, ...)
		standardGeneric("expand"))
}	

setMethod('expand', signature(x='RasterLayer', y='ANY'), 
function(x, y, filename='', value=NA, ...) {

	test <- try ( y <- extent(y), silent=TRUE )
	if (class(test) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

	filename <- trim(filename)
	
	bndbox <- extent(y)
	res <- res(x)
# snap points to pixel boundaries
	xmn <- round(xmin(bndbox) / res[1]) * res[1]
	xmx <- round(xmax(bndbox) / res[1]) * res[1]
	ymn <- round(ymin(bndbox) / res[2]) * res[2]
	ymx <- round(ymax(bndbox) / res[2]) * res[2]
	
# only expanding here, not cutting
	xmn <- min(xmn, xmin(x))
	xmx <- max(xmx, xmax(x))
	ymn <- min(ymn, ymin(x))
	ymx <- max(ymx, ymax(x))
	
	outraster <- raster(x)
	bndbox <- extent(xmn, xmx, ymn, ymx)
	outraster <- setExtent(outraster, bndbox, keepres=TRUE)

	if (canProcessInMemory(outraster, 2)) {
		if (dataContent(x) != 'all') { 
			x <- readAll(x) 
		}
		d <- vector(length=ncell(outraster))
		d[] <- value
		cells <- cellsFromExtent(outraster, extent(x))
		d[cells] <- values(x)
		outraster <- setValues(outraster, d)	
		if (filename != '') {
			outraster <- writeRaster(outraster, filename=filename, datatype=dataType(x), ...)
		}
	} else { 
		if (filename == '') {
			filename <- rasterTmpFile()
									
		}

		startrow <- rowFromY(outraster, yFromRow(x, 1))
		endrow <- rowFromY(outraster, yFromRow(x, nrow(x)))
		startcol <- colFromX(outraster, xFromCol(x, 1))
		endcol <- colFromX(outraster, xFromCol(x, ncol(x)))
		
		pb <- pbCreate(nrow(outraster), type=.progress(...))
		d <- vector(length=ncol(outraster))
		d[] = value
		datatype <- dataType(x)
		xr <- 0
		for (r in 1:nrow(outraster)) {
			d[] <- NA
			if (r >= startrow & r <= endrow) {
				xr <- xr + 1
				d[startcol:endcol] <- getValues(x, xr)
			}
			outraster <- setValues(outraster, d, r)
			outraster <- writeRaster(outraster, filename=filename, datatype=datatype, ...)
			pbStep(pb, r) 			
		}
		pbClose(pb)
	} 
	return(outraster)
}
)


