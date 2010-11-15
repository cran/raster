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
	
	pts <- try(  rasterToPoints(r, fun=function(z){ z>0 } )[,1:2, drop=FALSE] )
	
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
	
	if (.doCluster() ) {
		cl <- .makeCluster()
		nodes <- min(nrow(out), length(cl)) # at least 1 row
		
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()
		
		clFun <- function(r) {
			vals <- getValues(x, r)
			i = which(is.na(vals))
			vals[] <- 0
			if (length(i) > 0) {
				xy[,2] <- yFromRow(out, r)
				for (c in i) {
					vals[c] <- min( pointDistance(xy[c,], pts, type=disttype) )
				}
			}
			return( vals )
		}
	
        for (i in 1:nodes) {
			sendCall(cl[[i]], clFun, i, tag=i)
		}

		if (filename=="") {
			for (r in 1:nrow(out)) {
				d <- recvOneData(cl)
				if (! d$value$success) {
					stop('cluster error')
				}
				v[,d$value$tag] <- d$value$value

				if ((nodes + r) <= out@nrows) {
					sendCall(cl[[d$node]], clFun, nodes+r, tag=nodes+r)
				}
				pbStep(pb)
			}
		} else {
			for (r in 1:nrow(out)) {
				d <- recvOneData(cl)
				if (! d$value$success) {
					stop('cluster error')
				}
				out <- writeValues(out, as.vector(d$value$value), d$value$tag)
				if ((nodes + r) <= out@nrows) {
					sendCall(cl[[d$node]], clFun, nodes+r, tag=nodes+r)
				}
				pbStep(pb, r)
			}
		}
		stopCluster(cl)
	
	} else {	
	
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
				out <- writeValues(out, vals, r)
			}
			pbStep(pb, r) 	
		}	
	}
	
	pbClose(pb)
	
	if (filename == "") { 
		out <- setValues(out, as.vector(v)) 
	} else {
		out <- writeStop(out)
	}
	return(out)
}
)

