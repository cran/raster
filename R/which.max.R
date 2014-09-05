# Author: Robert J. Hijmans
# Date :  June 2013
# Version 1.0
# Licence GPL v3

if (!isGeneric("which.max")) {
	setGeneric("which.max", function(x)
		standardGeneric("which.max"))
}	


if (!isGeneric("which.min")) {
	setGeneric("which.min", function(x)
		standardGeneric("which.min"))
}	



setMethod("which.max", "RasterLayer",  
	function(x) { 
		m <- maxValue(x, warn=FALSE)
		if (is.na(m)) {
			return(NA)
		}
		if (canProcessInMemory(x)) {
			v <- values(x)
			return(which( v >= m))
		}
		x <- x >= m - 0.000001
		pts <- rasterToPoints(x, function(y) y == 1)
		cellFromXY(x, pts[,1:2,drop=FALSE])
	} 
)



setMethod("which.min", "RasterLayer",  
	function(x) { 
		m <- minValue(x, warn=FALSE)
		if (is.na(m)) {
			return(NA)
		}
		if (canProcessInMemory(x)) {
			v <- values(x)
			return(which( v <= m))
		}
		xx <- x <= m + 0.000001
		pts <- rasterToPoints(xx, function(y) y == 1)
		cellFromXY(xx, pts[,1:2,drop=FALSE])
	} 
)


setMethod("which.min", "RasterStackBrick",  
	function(x) { 
		r <- raster(x)
		nl <- nlayers(x)
		if (canProcessInMemory(x)) {
			x <- values(x)
			i <- rowSums(is.na(x)) < nl
			y <- rep(NA, nrow(x))	
			if (sum(i) > 0) {
				y[i] <- apply(x[i,], 1, which.min)
			}	
			return( setValues(r, y) )
		} else {
			stop('not yet implemented for large objects')
		}
	} 
)
	
	
	
setMethod("which.max", "RasterStackBrick",  
	function(x) { 
		r <- raster(x)
		nl <- nlayers(x)
		if (canProcessInMemory(x)) {
			x <- values(x)
			i <- rowSums(is.na(x)) < nl
			y <- rep(NA, nrow(x))	
			if (sum(i) > 0) {
				y[i] <- apply(x[i,], 1, which.max)
			}	
			return( setValues(r, y) )
		} else {
			stop('not yet implemented for large objects')
		}
	} 
)
	
	