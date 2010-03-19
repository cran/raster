# R function for the raster package
# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3


setMethod('bbox', signature(obj='Raster'), 
	function(obj) {
		b <- extent(obj)
		bb <- matrix(ncol=2, nrow=2)
		colnames(bb) <- c("min","max")
		rownames(bb) <- c("s1","s2")
		bb[1,1] <- b@xmin
		bb[1,2] <- b@xmax
		bb[2,1] <- b@ymin
		bb[2,2] <- b@ymax
		return(bb)
	}	
)

