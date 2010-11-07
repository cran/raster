# Author: Robert J. Hijmans and Reinhard Krug
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('overlay', signature(x='RasterStackBrick', y='RasterStackBrick'), 
function(x, y, ..., fun, filename=""){ 
	
	if (missing(fun)) { 
		stop("you must supply a function 'fun'. E.g., 'fun=function(x,y){return(x+y)}'") 
	}

	rasters <- .makeRasterList(x, y, ..., unstack=FALSE)
	nl <- sapply(rasters, nlayers)
	un <- unique(nl)
	if (length(un) > 1) {
		stop('number of layers does not match')
	} 
		
	return(.overlayList(rasters, fun=fun, filename=filename, ...))
}
)

