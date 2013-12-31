# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.9
# Licence GPL v3



setMethod('extract', signature(x='Raster', y='SpatialPolygons'), 
function(x, y, fun=NULL, na.rm=FALSE, weights=FALSE, cellnumbers=FALSE, small=FALSE, df=FALSE, layer, nl, factors=FALSE, sp=FALSE, ...){ 

	localFun <- function(i, pp) {
			spbb <- bbox(pp)
			
			if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
				# do nothing; res[[i]] <- NULL
			} else {
				rc <- crop(rr, extent(pp)+addres)
				
				agg <- FALSE
				if (ncell(rc) < 100) {
					d <- pmax(ceiling(10/dim(r)[1:2]), c(1,1))
					rc <- disaggregate(rc, d)
					agg <- TRUE
				}
				if (weights) {
					rc <- .polygonsToRaster(pp, rc, getCover=TRUE, silent=TRUE)
					rc[rc==0] <- NA
					xy <- rasterToPoints(rc)
					weight <- xy[,3] / 100
					xy <- xy[,-3,drop=FALSE]
				} else {
					rc <- .polygonsToRaster(pp, rc, silent=TRUE)
					xy <- rasterToPoints(rc)[,-3,drop=FALSE]
				}
			
				if (length(xy) > 0)  {  # to catch holes or very small polygons
					if (weights) {
						value <- .xyValues(x, xy, layer=layer, nl=nl)
						if (cellnumbers) {
							cell <- cellFromXY(x, xy)
							v <- cbind(cell, value, weight)
							if (agg) {
								cn <- colnames(v)
								v <- aggregate(v[,3:ncol(v)], list(v[,1], v[,2]), FUN=sum)
								colnames(v) <- cn
							}
							r <- v
						} else {		
							v <- cbind(value, weight)						
							if (agg) {
								cn <- colnames(v)
								v <- aggregate(v[,2:ncol(v)], list(v[,1,drop=FALSE]), FUN=sum)
								colnames(v) <- cn
							}
							r <- v
						}
					} else if (cellnumbers) {
						value <- .xyValues(x, xy, layer=layer, nl=nl)
						cell <- cellFromXY(x, xy)
						v <- cbind(cell, value)		
						if (agg) {
							cn <- colnames(v)
							v <- aggregate(v[,2:ncol(v)], list(v[,1,drop=FALSE]), FUN=sum)
							colnames(v) <- cn
						}
						r <- v
					} else {
						v <- .xyValues(x, xy, layer=layer, nl=nl)
						if (agg) {
							cell <- cellFromXY(x, xy)
							v <- cbind(cell, v)
							vv <- aggregate(v[,2:ncol(v)], list(v[,1,drop=FALSE]), FUN=sum)
							if (ncol(v) > 2) {
								colnames(vv) <- colnames(v)
							}
							v <- vv[,-1]
						}
						r <- v
					}
				} else if (small) {
					ppp <- pp@polygons[[1]]@Polygons
					ishole <- sapply(ppp, function(z)z@hole)
					xy <- lapply(ppp, function(z)z@coords)
					xy <- xy[!ishole]
					if (length(xy) > 0) {
						cell <- unique(unlist(lapply(xy, function(z) cellFromXY(x, z))))
						value <- .cellValues(x, cell, layer=layer, nl=nl)
						if (weights) {
							weight=1
							r <- cbind(cell, value, weight)
						} else if (cellnumbers) {
							r <- cbind(cell, value)					
						} else {
							r <- value
						}
					} # else do nothing; res[[i]] <- NULL
			} 
		}
		r
	}


	px <- projection(x, asText=FALSE)
	comp <- .compareCRS(px, projection(y), unknown=TRUE)
	if (!comp) {
		.requireRgdal()
		warning('Transforming SpatialPolygons to the CRS of the Raster')
		y <- spTransform(y, px)
	}
	
	spbb <- bbox(y)
	rsbb <- bbox(x)
	addres <- max(res(x))
	npol <- length(y@polygons)
	res <- list()
	res[[npol+1]] <- NA

	if (!is.null(fun)) {
		cellnumbers <- FALSE
	    if (weights) {
			if (!is.null(fun)) {
				test <- try(slot(fun, 'generic') == 'mean', silent=TRUE)
				if (!isTRUE(test)) {
					warning('"fun" was changed to "mean"; other functions cannot be used when "weights=TRUE"' )
				}
			}	
		}
		if (sp) {
			df <- TRUE
		}
	} else {
		if (sp) {
			sp <- FALSE
			warning('argument sp=TRUE is ignored if fun=NULL')
		}
	}
	
	if (missing(layer)) {
		layer <- 1
	} else {
		layer <- max(min(nlayers(x), layer), 1)
	}
	if (missing(nl)) {
		nl <- nlayers(x) - layer + 1
	} else {
		nl <- max(min(nlayers(x)-layer+1, nl), 1)
	}
	
	
	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		if (df) {
			res <- data.frame(matrix(ncol=1, nrow=0))
			colnames(res) <- 'ID'
			return(res)
		}
		return(res[1:npol])
	}
	
	rr <- raster(x)
	
	pb <- pbCreate(npol, label='extract', ...)
	
	if (.doCluster()) {
		cl <- getCluster()
		on.exit( returnCluster() )
		nodes <- min(npol, length(cl)) 
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()
	
		clusterExport(cl, c('rsbb', 'rr', 'weights', 'addres', 'cellnumbers', 'small'), envir=environment())
		
        for (ni in 1:nodes) {
			sendCall(cl[[ni]], localFun, list(ni, y[ni,]), tag=ni)
		}
		
		for (i in 1:npol) {
			d <- recvOneData(cl)
			if (! d$value$success) {
				stop('cluster error at polygon: ', i)
			}
			res[[d$value$tag]] <- d$value$value
			ni <- ni + 1
			if (ni <= npol) {
				sendCall(cl[[d$node]], localFun, list(ni, y[ni,]), tag=ni)
			}
			pbStep(pb, ni)
		}
		
	} else {
		for (i in 1:npol) {
			res[[i]] <- localFun(i, y[i,1])
			pbStep(pb)
		}
	}
	res <- res[1:npol]
	pbClose(pb)

	if (! is.null(fun)) {
		if (weights) {
			if (nl > 1) {
				meanfunc <- function(x) {
					if (!is.null(x)) { 
			# some complexity here because differnt layers could 
			# have different NA cells
						w <- x[,nl+1]
						x <- x[,-(nl+1)]
						x <- x * w
						w <- matrix(rep(w, nl), ncol=nl)
						w[is.na(x)] <- NA
						w <- colSums(w, na.rm=TRUE)
						x <- apply(x, 1, function(X) { X / w } )
						res <- rowSums(x, na.rm=na.rm) 
					} else {
						return( NULL )
					}	
				}
				res <- t(sapply(res, meanfunc))
			} else {
			
				res <- sapply(res, function(x) if (!is.null(x)){ sum(apply(x, 1, prod)) / sum(x[,2])} else NA  )
				
			}
			
		} else {
			i <- sapply(res, is.null)
			if (nl > 1) {
				j <- matrix(ncol=nl, nrow=length(res))
				j[!i] <- t(sapply(res[!i], function(x) apply(x, 2, FUN=fun, na.rm=na.rm)))
				colnames(j) <- names(x)[layer:(layer+nl-1)]
			} else {
				j <- vector(length=length(i))
				j[i] <- NA
				j[!i] <- sapply(res[!i], FUN=fun, na.rm=na.rm)
			}
			res <- j
		}
	}
	
	if (df) {
		if (!is.list(res)) {
			res <- data.frame(ID=1:NROW(res), res)
		} else {
			res <- data.frame( do.call(rbind, lapply(1:length(res), function(x) if (!is.null(res[[x]])) cbind(x, res[[x]]))) )
		}		

		lyrs <- layer:(layer+nl-1)
		if (cellnumbers) {
			nms <- c('ID', 'cell', names(x)[lyrs])
		} else {
			nms <- c('ID', names(x)[lyrs])
		}
		if (weights) {
			nms <- c(nms, 'weight')
		}
		colnames(res) <- nms
		
		if (any(is.factor(x)) & factors) {
			i <- ifelse(cellnumbers, 1:2, 1)
			v <- res[, -i, drop=FALSE]
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v[,1], layer))
			} else {
				v <- .insertFacts(x, v, lyrs)
			}
			res <- data.frame(res[,i,drop=FALSE], v)
		}
	}
	
	if (sp) {
		if (nrow(res) != npol) {
			warning('sp=TRUE is ignored because fun does not summarize the values of each polygon to a single number')
			return(res)
		}
		
		if (! .hasSlot(y, 'data') ) {
			y <- SpatialPolygonsDataFrame(y, res[, -1, drop=FALSE])
		} else {
			y@data <- cbind(y@data, res[, -1, drop=FALSE])
		}
		return(y)
	}

	res
}
)

#extract(r, p, weights = TRUE, cellnumbers=T)

