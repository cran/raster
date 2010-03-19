# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("distance")) {
	setGeneric("distance", function(x, ...)
		standardGeneric("distance"))
}	


setMethod('distance', signature(x='RasterLayer'), 

function(x, filename='', ...) {

	r = edge(x, classes=FALSE, type='inner', asNA=TRUE, progress=.progress(...)) 
	
	pts <- try(  rasterToPoints(r, fun=function(z){z>0})[,1:2, drop=FALSE] )
	
	if (class(pts) == "try-error") {
		return( .distanceRows(x, filename=filename, ...) )
	}

	if (nrow(pts) == 0) {
		stop('RasterLayer has no NA cells (for which to compute a distance)')
	}

	out <- raster(x)
	
	if (.couldBeLonLat(out)) { disttype <- 'GreatCircle' } else { disttype <- 'Euclidean' }
	                                                                        
	filename <- trim(filename)
	if (!canProcessInMemory(out, 2) && filename == '') {
		filename <- rasterTmpFile()
								
	}
	xy <- xFromCol(out, 1:ncol(out))
	xy <- cbind(xy, NA)
	
	if (filename == '') {
		v <- matrix(ncol=nrow(out), nrow=ncol(out))
 	} else {
		out <- writeStart(out, filename, ...)
	}
	
	pb <- pbCreate(nrow(out), type=.progress(...))
	for (r in 1:nrow(out)) {	
		vals <- getValues(x, r)
		i = which(is.na(vals))
		vals[] <- 0
		if (length(i) > 0) {
			xy[,2] <- yFromRow(out, r)
			for (c in i) {
				vals[c] <- min( pointDistance(xy[c,], pts, type=disttype) )
			}
		}
		if (filename == "") {
			v[,r] <- vals
		} else {
			writeValues(out, vals, r)
		}
		pbStep(pb, r) 	
	}	
	pbClose(pb)
	
	if (filename == "") { 
		out <- setValues(out, as.vector(v)) 
	} else {
		writeStop(out)
	}
	return(out)
}
)

