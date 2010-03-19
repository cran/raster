# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : November 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("mask")) {
	setGeneric("mask", function(x, mask, ...)
		standardGeneric("mask"))
}	


setMethod('mask', signature(x='RasterLayer', mask='RasterLayer'), 
function(x, mask, filename="", ...){ 

	compare(x, mask)
	
	if (dataContent(x) == 'all' & dataContent(mask)=='all') {
		x[is.na(mask)] <- NA
		return(x)
	} else if (canProcessInMemory(x, 3)) {
		if (dataContent(x) != 'all') { x <- readAll(x) }
		if (dataContent(mask) != 'all') { mask <- readAll(mask) }
		x[is.na(mask)] <- NA
		return(x)
	} else {
		out <- raster(x)
		vv <- matrix(ncol=nrow(out), nrow=ncol(out))
		filename <- trim(filename)
		if (!canProcessInMemory(out, 1) & filename=='') {
			filename <- rasterTmpFile()
		}

		if (filename == '') {
			v <- matrix(ncol=nrow(out), nrow=ncol(out))
		} else {
			out <- writeStart(out, filename=filename, ...)
		}
	
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, type=.progress(...))

		for (i in 1:tr$n) {
			v <- getValuesBlock( x, row=tr$row[i], nrows=tr$nrows[i] )
			m <- getValuesBlock( mask, row=tr$row[i], nrows=tr$nrows[i] )
			v[is.na(m)] <- NA
			if (filename != '') {
				writeValues(out, v, tr$row[i])
			} else {
				vv <- matrix(vv, nrow=ncol(outRaster))
				cols <- tr$row[i]:(tr$row[i]+dim(vv)[2]-1)	
				v[,cols] <- vv
			}
			pbStep(pb, i) 
		} 
		pbClose(pb)			

		if (filename == '') {
			out <- setValues(out, as.vector(vv))
		} else {
			out <- writeStop(out)
		}
		return(out)
	}
}
)


