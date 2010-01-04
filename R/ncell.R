# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("ncell")) {
	setGeneric("ncell", function(x)
		standardGeneric("ncell"))
}	

setMethod('ncell', signature(x='Raster'), 
	function(x) {
		return(as.numeric(x@ncols) * x@nrows)
	}
)


setMethod('ncell', signature(x='ANY'), 
	function(x) {
		NROW(x) * NCOL(x)
	}
)

