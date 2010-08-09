# Author: Robert J. Hijmans and Reinhard Krug
# International Rice Research Institute
# Date :  June 2008
# Version 0.9
# Licence GPL v3



setMethod('overlay', signature(x='RasterStackBrick', y='missing'), 
function(x, y, fun, indices=1:nlayers(x), filename="", ...){ 
	
	
	indices <- round(indices)
	if (min(indices) < 1) {	stop('indices should be >= 1') }
	if (max(indices) > nlayers(x)) {	stop('indices should be <= nlayers(x)') }
	
	rasters <- list()
	for (i in 1:length(indices)) {
		rasters[i] <- raster(x, indices[i])
	}
	
	if (missing(fun)) { 
		stop("you must supply a function 'fun'. E.g., 'fun=function(x,y){return(x+y)}'") 
	}
	
	if (length(fun) == 1) {

		return(.overlayList(rasters, fun=fun, filename=filename, ...))
	
	} else {
		if (filename != "" &&  (length(filename) != length(fun)) ) {
			stop('you must provide a filename for each function if you provide multiple functions')
		}
		
		# the idea is to optimize this, by reading all (row) data only once.... 
		res <- list()
		for (i in 1:length(fun)) {
			res[i] <- ( .overlayList(rasters, fun=fun, filename=filename, ...))
		}
		return(res)
	}
}
)

