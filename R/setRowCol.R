# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('dim', signature(x='BasicRaster'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x)))}
)

setMethod('nrow', signature(x='BasicRaster'), 
	function(x){ return(x@nrows)}
)

setMethod('ncol', signature(x='BasicRaster'), 
	function(x){ return(x@ncols) }
)


'rowcol<-' <- function(x, value) {
	if (! inherits(x, 'Raster') ) stop('x is an object of the wrong class. Should be Raster*')
	if (class(x) == 'RasterStack')  stop('Cannot change parameters of a RasterStack')
	
	if (length(value) == 1) {
		value <- c(value, ncol(x))
	}
	value <- as.integer(pmax(round(value), c(1,1)))
	
	if (value[1] != nrow(x) | value[2] != ncol(x)) {
		x <- clearValues(x)
	}

	x@nrows <- value[1]
	x@ncols <- value[2]
	return(x)
}


