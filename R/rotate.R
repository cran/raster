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
		xr <- xmax(x) - xmin(x)
		hx <- xr / 2
		r1 <- crop(x, extent(xmin(x), hx, ymin(x), ymax(x)))
		r2 <- crop(x, extent(hx, xmax(x), ymin(x), ymax(x)))
		xmax(r2) <- xmax(r2) - xr
		xmin(r2) <- xmin(r2) - xr
		m <- merge(r1, r2, ...)	
		return(m)
	}
)

