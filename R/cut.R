# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric("cut")) {
	setGeneric("cut", function(x, ...)
		standardGeneric("cut"))
}	

setMethod('cut', signature(x='Raster'), 

function(x, ..., filename='', format, datatype='INT2S', overwrite, progress)  {
	
	if (! hasValues(x) ) { 
		warning('x has no values, nothing to do')
		return(x) 
	}
	
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }

	nl <- nlayers(x)
	if (nl == 1) { out <- raster(x)
	} else { out <- brick(x, values=FALSE) }	
	
	if (canProcessInMemory(out, 2)) {

		if (nl > 1) {
			values(out) <- apply(getValues(x), 2, function(x) as.numeric(cut(x, ...)))
		} else {
			values(out) <- as.numeric(cut(getValues(x), ...))
		}
		if ( filename != "" ) { 
			out <- writeRaster(out, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress )
		}
		return(out)
				
	} else {

		if (filename == '') { filename <- rasterTmpFile() }

		out <- writeStart(out, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress )
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, type=progress)

		if (nl > 1) {
			for (i in 1:tr$n) {
				res <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				res <- apply(res, 2, function(x) as.numeric(cut(x, ...)))
				out <- writeValues(out, res, tr$row[i])
				pbStep(pb, i)
			}
		} else {
			for (i in 1:tr$n) {
				res <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				res <- as.numeric(cut(res, ...))
				out <- writeValues(out, res, tr$row[i])
				pbStep(pb, i)
			}
		}
		
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}
}
)

