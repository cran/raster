# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2010
# Version 1.0
# Licence GPL v3


setMethod('as.matrix', signature(x='RasterLayer'), 
function(x, ...){ 
	return( getValues(x, format='matrix') )
})


setMethod('as.matrix', signature(x='Raster'), 
function(x, ...){ 
	return( getValues(x) )
})

