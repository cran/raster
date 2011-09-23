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
	if (! missing(j)) { warning('second index is ignored') }
	subset(x, i, drop=drop)
})
