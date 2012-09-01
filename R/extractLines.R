# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.9
# Licence GPL v3



setMethod('extract', signature(x='Raster', y='SpatialLines'), 
function(x, y, fun=NULL, na.rm=FALSE, cellnumbers=FALSE, df=FALSE, layer, nl, factors=FALSE, ...){ 

	px <- projection(x, asText=FALSE)
	comp <- .compareCRS(px, projection(y), unknown=TRUE)
	if (!comp) {
		.requireRgdal()
		warning('Transforming SpatialLines to the CRS of the Raster')
		y <- spTransform(y, px)
	}

	spbb <- bbox(y)
	rsbb <- bbox(x)
	addres <- 2 * max(res(x))
	nlns <- length( y@lines )
	res <- list()
	res[[nlns+1]] = NA

	if (missing(layer)) {
		layer <- 1
	}
	if (missing(nl)) {
		nl <- nlayers(x)
	}	
	
	if (!is.null(fun)) {
		cellnumbers <- FALSE
	}
	
	if (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) {
		if (df) {
			res <- matrix(ncol=1, nrow=0)
			colnames(res) <- 'ID'
			return(res)
		} else {
			return(res[1:nlns])
		}
	}
	
	rr <- raster(x)
	cn <- names(x)
	
	pb <- pbCreate(nlns, ...)
	
	
	if (.doCluster()) {
		cl <- getCluster()
		on.exit( returnCluster() )
		nodes <- min(nlns, length(cl)) 
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()

		clFun <- function(i) {
			pp <- y[i,]
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
			pp <- y[i,]
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
	
	if (! is.null(fun)) {
		i <- sapply(res, is.null)
		if (nlayers(x) > 1) {
			j <- matrix(ncol=nlayers(x), nrow=length(res))
			j[!i] <- t(sapply(res[!i], function(x) apply(x, 2, fun, na.rm=na.rm)))
			colnames(j) <- names(x)
		} else {
			j <- vector(length=length(i))
			j[i] <- NA
			j[!i] <- sapply(res[!i], fun, na.rm)
		}
		res <- j
	}
	
	if (df) {
		if (!is.list(res)) {
			res <- data.frame(ID=1:NROW(res), res)
		} else {
			res <- data.frame( do.call(rbind, sapply(1:length(res), function(x) if (!is.null(res[[x]])) cbind(x, res[[x]]))) )
		}		

		lyrs <- layer:(layer+nl-1)
		colnames(res) <- c('ID', names(x)[lyrs])
		
		if (any(is.factor(x)) & factors) {
			v <- res[, -1, drop=FALSE]
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v[,1], layer))
			} else {
				v <- .insertFacts(x, v, lyrs)
			}
			res <- data.frame(res[,1,drop=FALSE], v)
		}
	}
	res
}
)


