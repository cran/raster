# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

distanceFromPoints <- function(object, xy, filename='', ...) {
	
	pts <- .pointsToMatrix(xy)
	rm(xy)

	filename <- trim(filename)
	
	if (.couldBeLonLat(object)) { 
		longlat=TRUE 
	} else { 
		longlat=FALSE 
	}
	                                                                        
	rst <- raster(object)
	if (!canProcessInMemory(rst, 2) && filename == '') {
		filename <- rasterTmpFile()			
	}

	xy <- xFromCol(rst, 1:ncol(rst))
	xy <- cbind(xy, NA)
	
	arow <- rep(NA, ncol(rst))
	
	if (filename == '') {
		v <- matrix(ncol=nrow(rst), nrow=ncol(rst))
	} else {
		rst <- writeStart(rst, filename=filename, ...)
	}
	
	pb <- pbCreate(nrow(rst), type=.progress(...))

	for (r in 1:nrow(rst)) {	
		vals <- arow
		xy[,2] <- yFromRow(rst, r)
		for (c in 1:length(xy[,1])) {
			vals[c] <- min( pointDistance(xy[c, ], pts, longlat=longlat) )
		}
		if (filename == "") {
			v[,r] <- vals
		} else {
			rst <- writeValues(rst, vals, r)
		}
		pbStep(pb, r) 	
	}	
	pbClose(pb)
	
	if (filename == "") { 
		rst <- setValues(rst, as.vector(v)) 
	} else {
		rst <- writeStop(rst)
	}
	return(rst)
}


