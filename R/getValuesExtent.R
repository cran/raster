# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("getValuesExtent")) {
	setGeneric("getValuesExtent", function(x, e, ...)
		standardGeneric("getValuesExtent"))
}	



setMethod('getValuesExtent', signature(x='Raster', e='Extent'), 
 	function(x, e) {
		e <- intersectExtent(e, x)
		e <- alignExtent(e, x)
		row = rowFromY(x, e@ymax)
		lastrow = rowFromY(x, e@ymin)
		nrows = lastrow-row+1
		col = colFromX(x, e@xmin)
		lastcol = colFromX(x, e@xmax)
		ncols = lastcol-col+1
		return (  getValuesBlock(x, row, nrows, col, ncols)  )
	}
	
)

