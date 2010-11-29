# Author: Robert J. Hijmans 
# contact: r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


setMethod("modal", signature(x='Raster'),
	function(x, ..., ties='random', na.rm=FALSE){
		rasters <- .makeRasterList(x, list(...))
		fun <- function(x){modal(x, ties=ties)}
		return( .summaryRasters(rasters, fun, 'modal', na.rm=na.rm) )
	}
)

