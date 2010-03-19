# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("getValues")) {
	setGeneric("getValues", function(x, row, nrows, ...)
		standardGeneric("getValues"))
}	

setMethod("getValues", signature(x='RasterLayer', row='missing', nrows='missing'), 
function(x, format='') {
	
	if (dataContent(x) != "all") {
		if (dataSource(x) == 'disk') {
			x <- readAll(x)
		} else {
			x <- setValues(x, rep(NA, ncell(x)))
		}
	}
	
	if (format=='matrix') { 
		return(matrix(x@data@values, ncol=x@ncols, nrow=x@nrows, byrow=TRUE)) 
	} else {
		return(x@data@values) 
	}
}
)

setMethod("getValues", signature(x='RasterBrick', row='missing', nrows='missing'), 
function(x) {
	if (dataContent(x) != "all") {
		if (dataSource(x) == 'disk') {
			x <- readAll(x)
		} else {
			stop('no values available')
		}
	}
	colnames(x@data@values) <- layerNames(x)
	x@data@values
}
)


setMethod("getValues", signature(x='RasterStack', row='missing', nrows='missing'), 
function(x) {
	m <- matrix(nrow=ncell(x), ncol=nlayers(x))
	colnames(m) <- layerNames(x)
	for (i in 1:nlayers(x)) {
		m[,i] <- getValues(x@layers[[i]])
	}
	m
}
)

