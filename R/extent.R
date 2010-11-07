# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3



if (!isGeneric("extent")) {
	setGeneric("extent", function(x, ...)
		standardGeneric("extent"))
}	

setMethod('extent', signature(x='Extent'), 
	function(x){ return(x) }
)

setMethod('extent', signature(x='BasicRaster'), 
	function(x){ return(x@extent) }
)

setMethod('extent', signature(x='Spatial'), 
	function(x){ 
		bndbox <- bbox(x)
		e <- new('Extent')
		e@xmin <- bndbox[1,1]
		e@xmax <- bndbox[1,2]
		e@ymin <- bndbox[2,1]
		e@ymax <- bndbox[2,2]
		return(e) 
	}
)

setMethod('extent', signature(x='matrix'), 
	function(x){ 
		d <- dim(x)
		if (min(d) < 2) {
			stop('matrix should have dimensions of at least 2 by 2') }		
		if (d[2] > 2) {
			stop('matrix should not have more than 2 columns') }		
		e <- new('Extent')
		if (nrow(x) == 2) {
		# assuming a 'sp' bbox object
			e@xmin <- min(x[1,])
			e@xmax <- max(x[1,])
			e@ymin <- min(x[2,])
			e@ymax <- max(x[2,])
		} else {
			a = apply(x, 2, range, na.rm=TRUE)
			e@xmin <- a[1,1]
			e@xmax <- a[2,1]
			e@ymin <- a[1,2]
			e@ymax <- a[2,2]
		}
		return(e)
	}
)
	
setMethod('extent', signature(x='numeric'), 
	function(x, ...){ 
		dots <- unlist(list(...))
		x <- c(x, dots)
		if (length(x) < 4) {
			stop('insufficient number of elements (should be 4)')
		}
		if (length(x) > 4) {
			warning('more elements than expected (should be 4)')
		}
		e <- new('Extent')
		e@xmin <- x[1]
		e@xmax <- x[2]
		e@ymin <- x[3]
		e@ymax <- x[4]
		return(e)
	}	
)

