# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("cover")) {
	setGeneric("cover", function(x, y, ...)
		standardGeneric("cover"))
}	

setMethod('cover', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, ..., filename='', format, datatype, overwrite, progress){ 

	rasters <- .makeRasterList(x, y, ...)
	compare(rasters)
		
	outRaster <- raster(x)

	if (missing(format)) {
		format <- .filetype()
	} 
	if (missing(overwrite)) {
		overwrite <- .overwrite()
	}
	if (missing(progress)) {
		progress <- .progress()
	}
	
	if (missing(datatype)) {
# check for boolean data?
		isInt <- TRUE
		for (i in 1:length(rasters)) {
			dtype <- .shortDataType(rasters[[i]]@file@datanotation)
			if (dtype != 'INT') {
				isInt <- FALSE
			}
		}
		if (isInt) { 
			datatype  <- 'INT4S'
		} else { 
			datatype <- 'FLT4S'
		}
	}
	filename <- trim(filename	)
	if (!canProcessInMemory(x, length(rasters)+2) && filename == '') {
		filename <- rasterTmpFile()
								
	}
	
	if (filename == '') {
		v <- matrix(ncol=nrow(outRaster), nrow=ncol(outRaster))
	} else {
		outRaster <- writeStart(outRaster, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress )
	}
	
	tr <- blockSize(outRaster, length(rasters))
	pb <- pbCreate(tr$n, type=.progress())

	for (i in 1:tr$n) {

		vv <- getValuesBlock( rasters[[1]], row=tr$row[i], nrows=tr$nrows[i] )

		for (j in 2:length(rasters)) {
			v2 <- getValuesBlock(rasters[[j]], row=tr$row[i], nrows=tr$nrows[i])
			vv[is.na(vv)] <- v2[is.na(vv)] 
		}	

		if (filename == "") {
			vv <- matrix(vv, nrow=ncol(outRaster))
			cols <- tr$row[i]:(tr$row[i]+dim(vv)[2]-1)	
			v[,cols] <- vv
		} else {
			writeValues(outRaster, vv, tr$row[i])
		}
		pbStep(pb, i) 
	}
	pbClose(pb)

	if (filename == "") {
		outRaster <- setValues(outRaster, as.vector(v))
	} else {
		outRaster <- writeStop(outRaster)
	}

	return(outRaster)
}
)

