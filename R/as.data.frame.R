# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : July 2011
# Version 1.0
# Licence GPL v3



if (!isGeneric("as.data.frame")) {
	setGeneric("as.data.frame", function(x, row.names = NULL, optional = FALSE, ...)
		standardGeneric("as.data.frame"))
}	


setMethod('as.data.frame', signature(x='RasterLayer'), 
	function(x, row.names = NULL, optional = FALSE, ...) {
		vname <- layerNames(x)
		x <- matrix(values(x), ncol=1)
		colnames(x) <- vname
		as.data.frame(x, row.names=row.names, optional=optional, ...)
	}
)

setMethod('as.data.frame', signature(x='RasterStackBrick'), 
	function(x, row.names = NULL, optional = FALSE, ...) {
		as.data.frame(values(x), row.names=row.names, optional=optional, ...)
	}
)
