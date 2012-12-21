# Author: Robert J. Hijmans
# Date :  September 2012
# Version 1.0
# Licence GPL v3

if (!isGeneric("barplot")) {
	setGeneric("barplot", function(height,...)
		standardGeneric("barplot"))
}	


setMethod('barplot', 'RasterLayer', 
	function(height, maxpixels=1000000, digits=0, breaks=NULL, col=rainbow, ...)  {
		x <- sampleRegular(height, maxpixels)
		if (!is.null(digits)) {
			x <- round(x, digits)
		}
		if (!is.null(breaks)) {
			x <- cut(x, breaks)
		}
		x <- table(x)
		if (is.function(col)) {
			col <- col(length(x))
		}
		barplot(x, col=col, ...)
	}
)
