# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: November 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("which")) {
	setGeneric("which", function(x, arr.ind=FALSE)
		standardGeneric("which"))
}	


setMethod('which', signature(x='RasterLayer'), 
function(x, arr.ind=FALSE) {

		
	if (canProcessInMemory(x, 2)){
		if (arr.ind) {
			return(which(getValues(x)==TRUE))
		} else {
			x <- as.logical(x)
			x[is.na(x)] <- FALSE
			return(x)
		}
	} else {
		out <- raster(x)
		if (arr.ind) {
			vv <- vector()
		} else {
			filename <- rasterTmpFile()
			
			out <- writeStart(out, filename=filename, format=.filetype(), datatype='INT1S', overwrite=TRUE)
		}
		
		tr <- blockSize(out, n=2)
		pb <- pbCreate(tr$n, type=.progress() )	
		for (i in 1:tr$n) {
			v <- getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i] ) 
			
			if (arr.ind) {
				offs = (tr$row[i]-1) * out@ncols
				vv <- c(vv, which(v==TRUE) + offs)
			} else {
				v <- as.logical(v)
				v[is.na(v)] <- FALSE
				out <- writeValues(out, v, tr$row[i])
			}
			pbStep(pb, i)
		}
		pbClose(pb)
		
		
		if (arr.ind) { 
			return(vv)
		} else { 
			x <- writeStop(x)
			return(x) 
		}
	}
}
)

