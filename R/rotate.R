# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("rotate")) {
	setGeneric("rotate", function(x, ...)
		standardGeneric("rotate"))
}	


setMethod('rotate', signature(x='Raster'), 
	function(x, ...) {
		e <- extent(x)
		xrange <- e@xmax - e@xmin
		if (xrange < 350 | xrange > 370 | e@xmin < -10 | e@xmax > 370) {
			warning('this does not look like an appropriate object for this function')
		}
		hx <- e@xmin + xrange / 2
		r1 <- crop(x, extent(e@xmin, hx, e@ymin, e@ymax))
		r2 <- crop(x, extent(hx, e@xmax, e@ymin, e@ymax))
		r2@extent@xmin <- r2@extent@xmin - xrange
		r2@extent@xmax <- r2@extent@xmax - xrange
		ln <- layerNames(x)
		x <- merge(r1, r2, overlap=FALSE, ...)
		layerNames(x) <- ln
		return(x)
	}
)

