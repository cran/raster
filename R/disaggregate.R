# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

.hasmethod <- function(method, ...) {
	if (!missing(method)) { 
		if (!method %in% c('bilinear', 'weighted', 'pycnophylactic')) {
			stop('unknown "method". Should be "bilinear" or absent')
		}
		return(method)
	}
	return('')
}


if (!isGeneric("disaggregate")) {
	setGeneric("disaggregate", function(x, fact, ...)
		standardGeneric("disaggregate"))
}

setMethod('disaggregate', signature(x='Raster', fact='numeric'), 
function(x, fact, filename='', ...) {

	method <- .hasmethod(...)
	
	if (length(fact)==1) {
		fact <- round(fact)
		if (fact == 1) 	return(x)
		if (fact < 2) { stop('fact should be > 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- round(fact[1])
		yfact <- round(fact[2])
		if (xfact < 1) { stop('fact[1] should be > 0') } 
		if (yfact < 1) { stop('fact[2] should be > 0') }
		if (xfact == 1 & yfact == 1) { return(x) }
	} else {
		stop('length(fact) should be 1 or 2')
	}

	filename <- trim(filename)

	nl <- nlayers(x)
	if (nl > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)
	}
	
	dim(out) <- c(nrow(x) * yfact, ncol(x) * xfact) 
	layerNames(out) <- layerNames(x)
	
	if (! inherits(x, 'RasterStack')) {
		if (! inMemory(x)  & ! fromDisk(x) ) {
			return(out)
		}
	}
	
	if (method=='bilinear') {
		return(resample(x, out, ...))
	} 
	
	
	if (canProcessInMemory(out, 3)) { 
	
		cols <- rep(rep(1:ncol(x), each=xfact), times=nrow(x)*yfact)
		rows <- rep(1:nrow(x), each=ncol(x)*xfact*yfact)
		cells <- cellFromRowCol(x, rows, cols)
		x <- getValues(x)

		if (is.matrix(x)) {
			x <- x[cells, ]
		} else {
			x <- x[cells]
		}
		

		out <- setValues(out, x)

		if (filename != '') {
			out <- writeRaster(out, filename=filename,...)
		}
		
	} else { 

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, ...)
		out <- writeStart(out, filename=filename, datatype=dataType(x), ...)

		if (nl > 1) {
			cols <- rep(1:ncol(x), each=xfact)
			rows <- rep(1:tr$nrow[1], each=yfact)
			for (i in 1:tr$n) {
				if (i == tr$n) {
					cols <- rep(1:ncol(x), each=xfact)
					rows <- rep(1:tr$nrow[1], each=yfact)
				}
				rown <- (tr$nrow[i]-1) * xfact + 1
				v <- getValues(x, tr$row[i], tr$nrows[i])
				v <- v[rows, cols]
				out <- writeValues(out, v, rown)
				pbStep(pb, r)
			}
		} else {
			for (i in 1:tr$n) {
				v <- getValues(x, tr$row[i], tr$nrows[i])
				v <- rep(rep(v, each=xfact), yfact)
				rown <- (tr$nrow[i]-1) * xfact + 1
				out <- writeValues(out, v, rown)
				pbStep(pb, i)
			}
		}

		out <- writeStop(out)
		pbClose(pb)
	}

	return(out)
}
)

