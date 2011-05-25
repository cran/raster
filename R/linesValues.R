# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3




lineValues <- function(lns, x, ...) {
	stop('function no longer available. Please use "extract"')
}


.lineValues <- function(x, lns, fun, na.rm=FALSE, cellnumbers=FALSE, layer, nl, ...) {
	spbb <- bbox(lns)
	rsbb <- bbox(x)
	addres <- 2 * max(res(x))
	nlns <- length( lns@lines )
	res <- list()
	res[[nlns+1]] = NA

	if (missing(layer)) {
		layer <- 1
	}
	if (missing(nl)) {
		nl <- nlayers(x)
	}	
	
	if (!missing(fun)) {
		cellnumbers <- FALSE
	}
	
	if (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) {
		return(res[1:nlns])
	}
	
	rr <- raster(x)
	cn <- layerNames(x)
	if (cn == "") { cn <- paste('v', 1:nlayers(x), sep='') }
	
	pb <- pbCreate(nlns, type=.progress(...))
	
	
	if (.doCluster()) {
		cl <- getCluster()
		on.exit( returnCluster() )
		nodes <- min(nlns, length(cl)) 
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()

		clFun <- function(i) {
			pp <- lns[i,]
			spbb <- bbox(pp)
			if (! (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) ) {
				rc <- crop(rr, extent(pp)+addres)
				rc <- .linesToRaster(pp, rc, silent=TRUE)
				xy <- rasterToPoints(rc)[,-3,drop=FALSE]
				if (length(xy) > 0) { # always TRUE?
					r <- .xyValues(x, xy, layer=layer, nl=nl)
					if (cellnumbers) {
						r <- cbind(cellFromXY(rr, xy), r)
						colnames(r) <- c('cell', cn)
					}
				} else {
					r <- NULL
				}
			}
			r
		}
		
        for (ni in 1:nodes) {
			sendCall(cl[[ni]], clFun, ni, tag=ni)
		}
		
		for (i in 1:nlns) {
			d <- recvOneData(cl)
			if (! d$value$success) {
				stop('cluster error at polygon: ', i)
			}
			res[[d$value$tag]] <- d$value$value
			ni <- ni + 1
			if (ni <= nlns) {
				sendCall(cl[[d$node]], clFun, ni, tag=ni)
			}
			pbStep(pb)
		}	
	
	
	} else {
	
	
		for (i in 1:nlns) {
			pp <- lns[i,]
			spbb <- bbox(pp)
			if (! (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) ) {
				rc <- crop(rr, extent(pp)+addres)
				rc <- .linesToRaster(pp, rc, silent=TRUE)
				xy <- rasterToPoints(rc)[,-3,drop=FALSE]
				if (cellnumbers) {
					if (length(xy) > 0) { # always TRUE?
						v <- cbind(cellFromXY(rr, xy), .xyValues(x, xy, layer=layer, nl=nl))
						colnames(v) <- c('cell', cn)
						res[[i]] <- v
					}
				} else {
					if (length(xy) > 0) { # always TRUE?
						res[[i]] <- .xyValues(x, xy, layer=layer, nl=nl)
					}
				}
			} 
			pbStep(pb)
		}
	}
	
	res <- res[1:nlns]
	
	pbClose(pb)
	
	if (! missing(fun)) {
		i <- sapply(res, is.null)
		if (nlayers(x) > 1) {
			j <- matrix(ncol=nlayers(x), nrow=length(res))
			j[!i] <- t(sapply(res[!i], function(x) apply(x, 2, fun, na.rm=na.rm)))
			colnames(j) <- layerNames(x)
		} else {
			j <- vector(length=length(i))
			j[i] <- NA
			j[!i] <- sapply(res[!i], fun, na.rm)
		}
		res <- j
	}
	res
}



