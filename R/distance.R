# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("distance")) {
	setGeneric("distance", function(x, ...)
		standardGeneric("distance"))
}	


setMethod('distance', signature(x='RasterLayer'), 

function(x, filename='', doEdge=FALSE, ...) {

	if (doEdge) {
		r <- edge(x, classes=FALSE, type='inner', asNA=TRUE, progress=.progress(...)) 
		pts <- try(  rasterToPoints(r, fun=function(z){ z>0 } )[,1:2, drop=FALSE] )
	} else {
		pts <- try(  rasterToPoints(x, fun=function(z){ z>0 } )[,1:2, drop=FALSE] )
	}
	
	if (class(pts) == "try-error") {
		return( .distanceRows(x, filename=filename, ...) )
	}

	if (nrow(pts) == 0) {
		stop('RasterLayer has no NA cells (for which to compute a distance)')
	}

	out <- raster(x)
	
	if (.couldBeLonLat(out)) { 
		distfun <- .haversine
	} else { 
		distfun <- .planedist
	}
	                                                                        
	filename <- trim(filename)
	if (!canProcessInMemory(out) && filename == '') {
		filename <- rasterTmpFile()
								
	}
	
	if (filename == '') {
		v <- matrix(ncol=nrow(out), nrow=ncol(out))
 	} else {
		out <- writeStart(out, filename, ...)
	}
	
	pb <- pbCreate(nrow(out), type=.progress(...))

	xy <- xFromCol(out, 1:ncol(out))
	xy <- cbind(xy, NA)
	
	if (.doCluster() ) {
		cl <- getCluster()
		on.exit( returnCluster() )
		
		nodes <- min(nrow(out), length(cl)) # at least 1 row
		
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()
		
		clFun <- function(r) {
			vals <- getValues(x, r)
			i <- which(is.na(vals))
			vals[] <- 0
			if (length(i) > 0) {
				xy[,2] <- yFromRow(out, r)
				for (c in i) {
					vals[c] <- min( distfun(xy[c,1], xy[c,2], pts[,1], pts[,2]) ) 
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
				if (! d$value$success) { stop('cluster error') }
				v[,d$value$tag] <- d$value$value
				nr <- nodes + r
				if (nr <= out@nrows) {
					sendCall(cl[[d$node]], clFun, nr, tag=nr)
				}
				pbStep(pb)
			}
			out <- setValues(out, as.vector(v)) 
			
		} else {
			for (r in 1:nrow(out)) {
				d <- recvOneData(cl)
				if (! d$value$success) { stop('cluster error') }
				out <- writeValues(out, as.vector(d$value$value), d$value$tag)
				nr <- nodes + r
				if (nr <= out@nrows) {
					sendCall(cl[[d$node]], clFun, nr, tag=nr)
				}
				pbStep(pb, r)
			}
			out <- writeStop(out)
		}
	
	} else {	
	
		if (filename=="") {
			for (r in 1:nrow(out)) {	
				vals <- getValues(x, r)
				i <- which(is.na(vals))
				vals[] <- 0
				if (length(i) > 0) {
					xy[,2] <- yFromRow(out, r)
					for (c in i) {
						#vals[c] <- min( pointDistance(xy[c,], pts, longlat=longlat) )
						vals[c] <- min( distfun(xy[c,1], xy[c,2], pts[,1], pts[,2]) )
					}
				}
				v[,r] <- vals
				pbStep(pb, r) 	
			}
			out <- setValues(out, as.vector(v)) 
			
		} else {
			for (r in 1:nrow(out)) {	
				vals <- getValues(x, r)
				i <- which(is.na(vals))
				vals[] <- 0
				if (length(i) > 0) {
					xy[,2] <- yFromRow(out, r)
					for (c in i) {
#						vals[c] <- min( pointDistance(xy[c,], pts, longlat=longlat) )
						vals[c] <- min( distfun(xy[c,1], xy[c,2], pts[,1], pts[,2]) )
					}
				}
				out <- writeValues(out, vals, r)
				pbStep(pb, r) 	
			}
			out <- writeStop(out)
		}
	}
	pbClose(pb)
	
	return(out)
}
)

