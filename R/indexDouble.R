# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3

setMethod("[[", "RasterLayer",
function(x,i,j,...,drop=TRUE) {
	stop('this method is not implemented for RasterLayer objects, use single brackets instead')
})


setMethod("[[", "RasterStackBrick",
function(x,i,j,...,drop=TRUE) {
	if ( missing(i)) { stop('you must provide an index') }
	if (! is.numeric(i)) { stop('you must provide a numeric index') }
	if (! missing(j)) { warning('second index is ignored') }
	i <- round(i)
	nl <- nlayers(x)
	if (length(i) == 1) {
		if (i < 1) { stop('index should be >= 1') }
		if (i > nl) { stop('index should be < ', nl) }
		return(raster(x, i))
	} else {
		if (i[1] < 1) { stop('index should be >= 1') }
		if (i[1] > nl) { stop('index should be < ', nl) }
		s <- stack(raster(x, i[1]))
		for (j in i[-1]) {
			if (j < 1) { stop('index should be >= 1') }
			if (j > nl) { stop('index should be < ', nl) }
			s <- addLayer(s, raster(x, j))
		}
		return(s)
	}
})
