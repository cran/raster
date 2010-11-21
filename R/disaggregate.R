# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

.hasmethod <- function(method, ...) {
	if (!missing(method)) { 
		if (method=='bilinear') {
			return( TRUE )
		} else {
			warning('unknown "method". Should be "method=bilinear", or absent')
		}
	}
	return(FALSE)
}


if (!isGeneric("disaggregate")) {
	setGeneric("disaggregate", function(x, fact, ...)
		standardGeneric("disaggregate"))
}

setMethod('disaggregate', signature(x='Raster', fact='numeric'), 
function(x, fact, filename='', ...) {

	hasmethod <- .hasmethod(...)
	
	if (length(fact)==1) {
		fact <- round(fact)
		if (fact < 2) { stop('fact should be > 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- round(fact[1])
		yfact <- round(fact[2])
		if (xfact < 2) { stop('fact[1] should be > 1') } 
		if (yfact < 2) { stop('fact[2] should be > 1') }
	} else {
		stop('length(fact) should be 1 or 2')
	}

	filename <- trim(filename)
	
	if (inherits(x, 'RasterLayer')) {
		outRaster <- raster(x)
	} else {
		outRaster <- brick(x, values=FALSE)
	}
	
	dim(outRaster) <- c(nrow(x) * yfact, ncol(x) * xfact) 

	
	if (! inherits(x, 'RasterStack')) {
		if (! inMemory(x)  & ! fromDisk(x) ) {
			return(outRaster)
		}
	}
	
	if (hasmethod) {
		return(resample(x, outRaster, ...))
	}
	
	
	if (canProcessInMemory(outRaster, 3)) { 
	
		cols <- rep(rep(1:ncol(x), each=xfact), times=nrow(x)*yfact)
		rows <- rep(1:nrow(x), each=ncol(x)*xfact*yfact)
		cells <- cellFromRowCol(x, rows, cols)
		x <- getValues(x)
		if (is.matrix(x)) {
			x <- x[cells, ]
		} else {
			x <- x[cells]
		}
		outRaster <- setValues(outRaster, x)

		if (filename != '') {
			outRaster <- writeRaster(outRaster, filename=filename,...)
		}
		
	} else { 
		if (filename == '') {
			filename <- rasterTmpFile()						
		}
		cols <- rep(rep(1:ncol(x), each=xfact), times=yfact)

		pb <- pbCreate(nrow(x), type=.progress(...))
		outRaster <- writeStart(outRaster, filename=filename, datatype=dataType(x), ...)
		if (inherits(x, 'RasterLayer')) {
			for (r in 1:nrow(x)) {
				vals <- getValues(x, r)
				rown <- (r-1) * xfact + 1
				outRaster <- writeValues(outRaster, vals[cols], rown)
				pbStep(pb, r)
			}
		} else {
			for (r in 1:nrow(x)) {
				vals <- getValues(x, r)
				rown <- (r-1) * xfact + 1
				outRaster <- writeValues(outRaster, vals[cols, ], rown)
				pbStep(pb, r)
			}
		}

		outRaster <- writeStop(outRaster)
		pbClose(pb)
	}

	return(outRaster)
}
)

