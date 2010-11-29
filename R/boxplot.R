# Author: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  November 2010
# Version 1.0
# Licence GPL v3
 

if (!isGeneric("boxplot")) {
	setGeneric("boxplot", function(x, ...)
		standardGeneric("boxplot"))
}

setMethod('boxplot', signature(x='Raster'), 
	function(x, maxpixels=100000, ...) {
		nl <- nlayers(x)
		cn <- layerNames(x)
		if ( canProcessInMemory(x)) {
			x <- getValues(x)
		} else {
			warning('taking a sample of ', maxpixels, ' cells')
			x = sampleRegular(x, maxpixels)
		}	
		if (nl == 1) {
			x <- matrix(x)
		}
		colnames(x) <- cn
		boxplot(x, ...)
	}
)

