# Author: Robert J. Hijmans 
# International Rice Research Institute
# contact: r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3


setMethod("modal", signature(x='Raster'),
	function(x, ..., ties='random', na.rm=FALSE){
		rasters <- list(...)
		if (class(x) == 'RasterLayer') {
			if (length(rasters)==0) { 
				return(x) 
			}
		}
		rasters <- c(x, rasters)
		rm(x)
		for (i in 1:length(rasters)) {
			if (class(rasters[[i]]) == 'RasterStack') {
				r <- rasters[[i]]
				rasters <- rasters[-i]
				rasters <- c(rasters, unstack(r))
				rm(r)
			}
		}
		fun <- function(x){modal(x, ties=ties)}
		return( .summaryRasters(rasters, fun, 'modal', na.rm=na.rm) )
	}
)

