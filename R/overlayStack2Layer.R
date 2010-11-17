# Author: Robert J. Hijmans and Reinhard Krug
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('overlay', signature(x='RasterStackBrick', y='missing'), 
function(x, y, fun, filename="", ...){ 
	
	
	rasters <- list()
	for (i in 1:nlayers(x)) {
		rasters[i] <- raster(x, i)
	}
	
	if (missing(fun)) { 
		stop("you must supply a function 'fun'. E.g., 'fun=function(x,y){return(x+y)}'") 
	}
	
	if (length(fun) == 1) {
		return(.overlayList(rasters, fun=fun, filename=filename, ...))
	} else {
		# the idea is to optimize this, by reading all (row) data only once.... 
		res <- list()
		for (i in 1:length(fun)) {
			res[i] <- ( .overlayList(rasters, fun=fun, filename='', ...))
		}
		return(res)
	}
}
)

