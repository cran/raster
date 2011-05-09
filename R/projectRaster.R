# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


projectExtent <- function(object, crs) {
	if (! .requireRgdal() ) { stop('rgdal not available') }

	validObject(projection(object, asText=FALSE))
	validObject(projection(crs, asText=FALSE))
	projfrom <- projection(object)
	projto <- projection(crs)
	
#	rs <- res(object)
#	xmn <- object@extent@xmin - 0.5 * rs[1]
#	xmx <- object@extent@xmax + 0.5 * rs[1]
#	ymn <- object@extent@ymin - 0.5 * rs[2]
#	ymx <- object@extent@ymax + 0.5 * rs[2]
#	xha <- (xmn + xmx) / 2
#	yha <- (ymn + ymx) / 2
#	xy <- matrix(c(xmn, ymx, xha, ymx, xmx, ymx, xmn, yha, xha, yha, xmx, yha, xmn, ymn, xha, ymn, xmx, ymn), ncol=2, byrow=T)
	
	
	xy1 <- xyFromCell(object, cellFromCol(object, 1))
	xy1[,1] <- xy1[,1] - 0.5 * xres(object)
	xy1[1,2] <- xy1[1,2] + 0.5 * yres(object)
	xy1[nrow(object),2] <- xy1[nrow(object),2] + 0.5 * yres(object)
	
	xy2 <- xyFromCell(object, cellFromCol(object, ncol(object)))
	xy2[,1] <- xy2[,1] + 0.5 * xres(object)
	xy2[1,2] <- xy2[1,2] + 0.5 * yres(object)
	xy2[nrow(object),2] <- xy2[nrow(object),2] + 0.5 * yres(object)

	xy3 <- xyFromCell(object, cellFromRow(object, 1))
	xy3[,2] <- xy3[,2] + 0.5 * yres(object)
	xy3[1,1] <- xy3[1,1] - 0.5 * xres(object)
	xy3[ncol(object),1] <- xy3[ncol(object),1] + 0.5 * xres(object)
	
	xy4 <- xyFromCell(object, cellFromRow(object, nrow(object)))
	xy4[,2] <- xy4[,2] + 0.5 * yres(object)
	xy4[1,1] <- xy4[1,1] - 0.5 * xres(object)
	xy4[ncol(object),1] <- xy4[ncol(object),1] + 0.5 * xres(object)
	
	xy <- rbind(xy1, xy2, xy3, xy4)
	
	res <- .Call("transform", projfrom, projto, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
	
	x <- res[[1]]
	y <- res[[2]]
	xy <- cbind(x, y)
	xy <- subset(xy, !(is.infinite(xy[,1]) | is.infinite(xy[,2])) )
	x <- xy[,1]
	y <- xy[,2]
	
	if (length(y) == 0 | length(y) ==0) { stop("cannot do this transformation") }
	minx <- min(x)
	maxx <- max(x)
	if (maxx == minx) {
		maxx <- maxx + 0.5
		minx <- minx - 0.5
	}
	miny <- min(y)
	maxy <- max(y)
	if (maxy == miny) {
		maxy <- maxy + 0.5
		miny <- miny - 0.5
	}
	
	obj <- raster(extent(minx, maxx, miny,  maxy), nrows=nrow(object), ncols=ncol(object), crs=crs)
	return(obj)
}


.computeRes <- function(raster, crs) {
	x <- xmin(raster) + 0.5 * (xmax(raster) - xmin(raster))
	y <- ymin(raster) + 0.5 * (ymax(raster) - ymin(raster))
	res <- res(raster)
	x1 <- x - 0.5 * res[1]
	x2 <- x + 0.5 * res[1]
	y1 <- y - 0.5 * res[2]
	y2 <- y + 0.5 * res[2]
	xy <- cbind(c(x1, x2, x, x), c(y, y, y1, y2))
	pXY <- .Call("transform", projection(raster), crs, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
	pXY <- cbind(pXY[[1]], pXY[[2]])
	res <- c((pXY[2,1] - pXY[1,1]), (pXY[4,2] - pXY[3,2]))
	# abs should not be necessary, but who knows what a projection might do?
	abs( signif(res, digits=3) )
}


projectRaster <- function(from, to, res, crs, method="bilinear", filename="", ...)  {

	if (! .requireRgdal() ) { stop('rgdal not available') }

	validObject(projection(from, asText=FALSE))
	projfrom <- projection(from)
	if (projfrom == "NA") { stop("input projection is NA") }
	lonlat <- isLonLat(projfrom)
	
	if (missing(to)) {
		if (missing(crs)) {
			stop("'crs' argument is missing.")
		}
		crs2 <- paste(crs, "+over")
		to <- projectExtent(from, crs2)
		if (missing(res)) {
			res <- .computeRes(from, crs2)
		}
		res(to) <- res
		projto <- projection(to)

		# add some cells to capture curvature
		e <- extent(to)
		add <- min(5, min(dim(to)[1:2])/10) * max(res)
		e@ymin <- e@ymin - add
		e@ymax <- e@ymax + add
		e@xmin <- e@xmin - add
		e@xmax <- e@xmax + add
		if (!is.character(projto)) projto <- projto@projargs
		if (substr(projto, 1, 13) == "+proj=longlat") {
			e@xmin <- max(-180, e@xmin)
			e@xmax <- min(180, e@xmax)
			e@ymin <- max(-90, e@ymin)
			e@ymax <- min(90, e@ymax)
		}
		to <- expand(to, e)
	} else {
		projto <- projection(to)
		if (projto == "NA") { 
			if (missing(crs) | is.na(crs) | crs == 'NA' ) {
				stop("output projection is NA") 
			} else {
				projto <- projection(to) <- crs
			}
		} 
	}

	validObject(to)
	validObject(projection(to, asText=FALSE))

	if (identical(projfrom, projto)) {
		stop('projections of "from" and "to" are the same')
	}	

	if ( ! hasValues(from) ) {
		return(to)
	}

	pbb <- projectExtent(to, projection(from))
	bb <- intersectExtent(pbb, from)
	validObject(bb)

	bb <- extent(projectExtent(from, projection(to)))
	startrow <- max(rowFromY(to, bb@ymax) - 10, 1, na.rm=TRUE)
	endrow <- min(rowFromY(to, bb@ymin) + 10, to@nrows, na.rm=TRUE)
	startcol <- max(colFromX(to, bb@xmin) - 10, 1, na.rm=TRUE)
	endcol <- min(colFromX(to, bb@xmax) + 10, to@ncols, na.rm=TRUE)
		
	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') }
	if (method=='ngb') { method <- 'simple' } # for extract (.xyValues)

	filename <- trim(filename)
	
	nl <- nlayers(from)
	if ( nl == 1) {
		to <- raster(to)
	} else {
		to <- brick(to, values=FALSE)
		to@data@nlayers <- nl
	}

	if (!canProcessInMemory(to, n=nl*2) && filename == "") {
		filename <- rasterTmpFile()
	}

	inMemory <- filename == ""
	if (inMemory) {
		v <- matrix(NA, nrow=ncell(to), nlayers(from))
	} else {
		to <- writeStart(to, filename=filename, ...)
	}
	
	
	if (.doCluster()) {
		
		cl <- getCluster()
		on.exit( returnCluster() )

		nodes <- min(ceiling(to@nrows/10), length(cl)) # at least 10 rows per node
		
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()
		
		tr <- blockSize(to, minblocks=nodes)
		pb <- pbCreate(tr$n, type=.progress(...))

		clFun <- function(i) {
			r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)

			if (max(r) < startrow | min(r) > endrow) {
				vals <- rep(NA, times=tr$nrows[i] * to@ncols)
			} else {
				srow <- max(startrow, tr$row[i])
				erow <- min(endrow, (tr$row[i]+tr$nrows[i]-1))
				nrows <- erow-srow+1
				cells <- cellFromRowColCombine(to, srow:erow, startcol:endcol)
				xy <- xyFromCell(to, cells ) 
				xy <- .Call("transform", projto, projfrom, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
				xy <- cbind(xy[[1]], xy[[2]])
				cells <- cells - (tr$row[i] - 1) * to@ncols
				vals <- matrix(NA, nrow=tr$nrows[i] * to@ncols, ncol=nl)
				vals[cells,] <- .xyValues(from, xy, method=method)
			}
			return(vals)
		}
		
		# for debugging
		# clusterExport(cl,c("tr", "projto", "projfrom", "method", "from", "to"))
        for (i in 1:nodes) {
			sendCall(cl[[i]], clFun, i, tag=i)
		}
		        
		if (inMemory) {
			for (i in 1:tr$n) {
				pbStep(pb, i)
				d <- recvOneData(cl)
				if (! d$value$success) {
					stop('cluster error')
				}
				start <- cellFromRowCol(to, tr$row[d$value$tag], 1)
				end <- cellFromRowCol(to, tr$row[d$value$tag] + tr$nrows[d$value$tag]-1, to@ncols)
				v[start:end, ] <- d$value$value
				ni <- nodes+i
				if (ni <= tr$n) {
					sendCall(cl[[d$node]], clFun, ni, tag=ni)
				}
			}
			
		} else {
		
			for (i in 1:tr$n) {
				pbStep(pb, i)
				d <- recvOneData(cl)
				if (! d$value$success ) { stop('cluster error') }
				to <- writeValues(to, d$value$value, tr$row[d$value$tag])
				if ((nodes + i) <= tr$n) {
					sendCall(cl[[d$node]], clFun, nodes+i, tag=i)
				}
			}
		}	
		
	} else {
		# this seems to need smaller chunks
		#cz <- max(5, 0.1 * .chunksize() / nlayers(to))
		tr <- blockSize(to)
		
		pb <- pbCreate(tr$n, type=.progress(...))
		for (i in 1:tr$n) {
			r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
			if (max(r) < startrow | min(r) > endrow) {
				vals <- rep(NA, times=tr$nrows[i] * to@ncols)
			} else {
				srow <- max(startrow, tr$row[i])
				erow <- min(endrow, (tr$row[i]+tr$nrows[i]-1))
				cells <- cellFromRowColCombine(to, srow:erow, startcol:endcol)
				xy <- xyFromCell(to, cells ) 
				xy <- .Call("transform", projto, projfrom, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
				xy <- cbind(xy[[1]], xy[[2]])
				cells <- cells - (tr$row[i] - 1) * to@ncols
				vals <- matrix(NA, nrow=tr$nrows[i] * to@ncols, ncol=nl)
				vals[cells,] <- .xyValues(from, xy, method=method)
			}
			if (inMemory) {
				start <- cellFromRowCol(to, tr$row[i], 1)
				end <- cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, to@ncols)
				v[start:end, ] <- vals
			} else {
				to <- writeValues(to, vals, tr$row[i])
			}
			pbStep(pb)
		}
	}
	pbClose(pb)
	
	if (inMemory) {
		to <- setValues(to, v)
	} else {
		to <- writeStop(to)	
	}
	return(to)
}

