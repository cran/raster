# raster package
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

.hasmethod <- function(method, ...) {
	if (!missing(method)) { 
		if (method=='bilinear') {
			return( TRUE )
		}
	}
	return(FALSE)
}


if (!isGeneric("disaggregate")) {
	setGeneric("disaggregate", function(x, fact, ...)
		standardGeneric("disaggregate"))
}	

setMethod('disaggregate', signature(x='RasterLayer', fact='numeric'), 
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
	outraster <- raster(x)
	rowcol(outraster) <- c(nrow(x) * yfact, ncol(x) * xfact) 

	if (dataContent(x) != 'all' & dataSource(x) == 'ram') {
		return(outraster)
	}
	
	if (hasmethod) {
		return(resample(x, outraster, ...))
	}
	
	
	if ((!canProcessInMemory(outraster, 3)) && filename == '') {
		filename <- rasterTmpFile()
								
	}
	
	if ( filename == "" ) {
		if (dataContent(x) != 'all') {
			x <- readAll(x)
		}
		cols <- rep(rep(1:ncol(x), each=xfact), times=nrow(x)*yfact)
		rows <- rep(1:nrow(x), each=ncol(x)*xfact*yfact)
		cells <- cellFromRowCol(x, rows, cols)
		outraster <- setValues(outraster, values(x)[cells])
		
	} else { 
		# to speed up valuesRow
		if (dataContent(x) != 'all') { x <- clearValues(x) }
		v <- vector(length=0)
		cols <- rep(1:ncol(x), each=xfact)

				
		pb <- pbCreate(nrow(x), type=.progress(...))
		for (r in 1:nrow(x)) {
			vals <- getValues(x, r)
			for (i in 1:yfact) {
				outraster <- setValues(outraster, vals[cols], (r-1) * xfact + i)
				outraster <- writeRaster(outraster, filename=filename, ...)
			}	
			pbStep(pb, r)
		}
		pbClose(pb)
	}
	return(outraster)
}
)
