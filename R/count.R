

if (!isGeneric("count")) {
	setGeneric("count", function(x, ...)
		standardGeneric("count"))
}	


setMethod('count', signature(x='Raster'), 
function(x, value, digits=0, progress='', ...) {
	stop('this function is obsolete, use "freq" instead')
} )


