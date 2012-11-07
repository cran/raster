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
	nl <- sapply(rasters, nlayers)
	if (max(nl) > 1) {
		stop("Only single layer (RasterLayer) objects can be used if 'x' and 'y' have a single layer")
	} 
		
	outRaster <- raster(x)
	compareRaster(c(outRaster, rasters))
	
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }
	if (missing(datatype)) { 
		datatype <- unique(dataType(x))
		if (length(datatype) > 1) {
			datatype <- .commonDataType(datatype)
		}
	}
	if (canProcessInMemory(x, length(rasters) + 2)) {
	
		v <- getValues( rasters[[1]] )
		for (j in 2:length(rasters)) {
			v[is.na(v)] <- getValues(rasters[[j]])[is.na(v)]
		}	
		outRaster <- setValues(outRaster, v)
		if (filename != '') {
			outRaster <- writeRaster(outRaster, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress )
		}
		
	} else {
	
		if (filename == '') { filename <- rasterTmpFile() }
		outRaster <- writeStart(outRaster, filename=filename, format=format, datatype=datatype, overwrite=overwrite )
		tr <- blockSize(outRaster, length(rasters))
		pb <- pbCreate(tr$n, progress=progress, label='cover')
		for (i in 1:tr$n) {
			v <- getValues( rasters[[1]], row=tr$row[i], nrows=tr$nrows[i] )
			if (! is.matrix(v) ) {	v <- matrix(v, ncol=1) }		
			for (j in 2:length(rasters)) {
				vv <- getValues(rasters[[j]], row=tr$row[i], nrows=tr$nrows[i])
				v[is.na(v)] <- vv[is.na(v)] 
			}	
			outRaster <- writeValues(outRaster, v, tr$row[i])
			pbStep(pb, i) 
		}
		pbClose(pb)
		outRaster <- writeStop(outRaster)
	}
	return(outRaster)
}
)

