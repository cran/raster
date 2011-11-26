# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : November 2008
# Version 1.0
# Licence GPL v3


	
.xyValues <- function(object, xy, method='simple', buffer=NULL, fun=NULL, na.rm=TRUE, layer, nl, cellnumbers=FALSE, ...) { 

	nlyrs <- nlayers(object)
	if (nlyrs > 1) {
		if (missing(layer)) { layer <- 1 } 
		if (missing(nl)) { nl <- nlyrs } 
		layer <- min(max(1, round(layer)), nlyrs)
		nl <- min(max(1, round(nl)), nlyrs-layer+1)
	} else {
		layer <- 1
		nl <- 1
	}
	
	if (dim(xy)[2] != 2) {
		stop('xy should have 2 columns only.\nFound these dimensions: ', paste(dim(xy), collapse=', ') )
	}
		
	if (! is.null(buffer)) {
	if (method != 'simple') { warning('method argument is ignored when a buffer is used') }
		return( .xyvBuf(object, xy, buffer, fun, na.rm, layer=layer, nl=nl, cellnumbers=cellnumbers) )
	}

	if (method == 'bilinear') {
		return ( .bilinearValue(object, xy, layer=layer, n=nl) )

	} else if (method=='simple') {
		cells <- cellFromXY(object, xy)
		return( .cellValues(object, cells, layer=layer, nl=nl) )
			
	} else {
		stop('invalid "method" argument. Should be simple or bilinear.')
	}
}


