# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


projectExtent <- function(object, crs) {
	.requireRgdal()

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
	
	
	rows <- unique(c(seq(1,nrow(object), by=max(1, round(nrow(object)/50))), nrow(object)))
	cols <- unique(c(seq(1,ncol(object), by=max(1, round(ncol(object)/50))), ncol(object)))
	
	xy1 <- xyFromCell(object, cellFromRowCol(object, rows, 1))
	xy1[,1] <- xy1[,1] - 0.5 * xres(object)
	xy1[1,2] <- xy1[1,2] + 0.5 * yres(object)
	xy1[nrow(xy1),2] <- xy1[nrow(xy1),2] + 0.5 * yres(object)
	
	xy2 <- xyFromCell(object, cellFromRowCol(object, rows, ncol(object)))
	xy2[,1] <- xy2[,1] + 0.5 * xres(object)
	xy2[1,2] <- xy2[1,2] + 0.5 * yres(object)
	xy2[nrow(xy2),2] <- xy2[nrow(xy2),2] + 0.5 * yres(object)

	xy3 <- xyFromCell(object, cellFromRowCol(object, 1, cols))
	xy3[,2] <- xy3[,2] + 0.5 * yres(object)
	xy3[1,1] <- xy3[1,1] - 0.5 * xres(object)
	xy3[ncol(xy3),1] <- xy3[ncol(xy3),1] + 0.5 * xres(object)
	
	xy4 <- xyFromCell(object, cellFromRowCol(object, nrow(object), cols))
	xy4[,2] <- xy4[,2] + 0.5 * yres(object)
	xy4[1,1] <- xy4[1,1] - 0.5 * xres(object)
	xy4[ncol(xy4),1] <- xy4[ncol(xy4),1] + 0.5 * xres(object)
	
	
	# added for circumpolar data:
	if (nrow(object) > 75 & ncol(object) > 75) {
	
		rows <- c(seq(min(nrow(object), 25), nrow(object), by=50))
		cols <- c(seq(min(ncol(object), 25), ncol(object), by=50))
		xy5 <- xyFromCell(object, cellFromRowColCombine(object, rows, cols))
		
		xy <- rbind(xy1, xy2, xy3, xy4, xy5)
		
	} else {

		xy <- rbind(xy1, xy2, xy3, xy4)
	
	}
	

	
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

	.requireRgdal()

	validObject(projection(from, asText=FALSE))
	projfrom <- projection(from)
	if (projfrom == "NA") { stop("input projection is NA") }
	lonlat <- isLonLat(projfrom)
	
	if (missing(to)) {
		if (missing(crs)) {
			stop("'crs' argument is missing.")
		}
		projto <- crs
		to <- projectExtent(from, projto)
		if (missing(res)) {
			res <- .computeRes(from, projto)
		}
		res(to) <- res
		projection(to) <- crs

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
			} 
		} 
		
		e <- extent( projectExtent(from, projto) )
		add <- min(10, min(dim(to)[1:2])/10) * max(raster::res(to))
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
		
	}

	validObject(to)
	validObject(projection(to, asText=FALSE))

	if (identical(projfrom, projto)) {
		stop('projections of "from" and "to" are the same')
	}	

#	pbb <- projectExtent(to, projection(from))
#	bb <- intersect(extent(pbb), extent(from))
#	validObject(bb)

	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') }
	if (method=='ngb') { method <- 'simple' } # for extract (.xyValues)

	nl <- nlayers(from)
	if ( nl == 1) {
		to <- raster(to)
	} else {
		to <- brick(to, values=FALSE, nl=nl)
	}
	layerNames(to) <- layerNames(from)
	if ( ! hasValues(from) ) {
		warning("'from' has no cell values")
		return(to)
	}
	
	if (canProcessInMemory(to, n=nl*2)) {
		inMemory <- TRUE
	} else {
		inMemory <- FALSE
	}

	
	
	if (.doCluster()) {
		
		cl <- getCluster()
		on.exit( returnCluster() )

		nodes <- min(ceiling(to@nrows/10), length(cl)) # at least 10 rows per node
		
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()
		
		tr <- blockSize(to, minblocks=nodes)
		pb <- pbCreate(tr$n, ...)

		clFun <- function(i) {
			start <- cellFromRowCol(to, tr$row[i], 1)
			end <- start + tr$nrows[i] * ncol(to) - 1
			cells <- start:end
			xy <- xyFromCell(to, cells) 
			xy <- subset(xy, xy[,1] > e@xmin & xy[,1] < e@xmax)
			v <- matrix(nrow=length(cells), ncol=nl)
			if (nrow(xy) > 0) {
				ci <- match(cellFromXY(to, xy), cells)
				xy <- .Call("transform", projto, projfrom, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
				xy <- cbind(xy[[1]], xy[[2]])
				v[ci, ] <- .xyValues(from, xy, method=method)
			} 
			return(v)
		}
	
		# for debugging
		# clusterExport(cl,c("tr", "projto", "projfrom", "method", "from", "to"))
        for (i in 1:nodes) {
			sendCall(cl[[i]], clFun, i, tag=i)
		}
		        
		if (inMemory) {
			v <- matrix(nrow=ncell(to), ncol=nlayers(from))

			for (i in 1:tr$n) {
				pbStep(pb, i)
				d <- recvOneData(cl)
				if (! d$value$success) {
					stop('cluster error')
				}
				start <- cellFromRowCol(to, tr$row[d$value$tag], 1)
				end <- start + tr$nrows[d$value$tag] * ncol(to) - 1
				v[start:end, ] <- d$value$value
				ni <- nodes+i
				if (ni <= tr$n) {
					sendCall(cl[[d$node]], clFun, ni, tag=ni)
				}
			}
			
			to <- setValues(to, v)
			if (filename != '') {
				to <- writeRaster(to, filename, ...)
			}
			return(to)
			
		} else {
			to <- writeStart(to, filename=filename, ...)

			for (i in 1:tr$n) {
				pbStep(pb, i)
				d <- recvOneData(cl)
				if (! d$value$success ) { stop('cluster error') }
				to <- writeValues(to, d$value$value, tr$row[d$value$tag])
				ni <- nodes+i
				if (ni <= tr$n) {
					sendCall(cl[[d$node]], clFun, ni, tag=ni)
				}
			}
			pbClose(pb)
			to <- writeStop(to)	
			return(to)
		}	
		
	} else {
		# this seems to need smaller chunks
		#cz <- max(5, 0.1 * .chunksize() / nlayers(to))
		
		
		if (inMemory) {
			
			xy <- coordinates(to) 
			xy <- subset(xy, xy[,1] > e@xmin & xy[,1] < e@xmax)
			cells <- cellFromXY(to, xy)
			xy <- .Call("transform", projto, projfrom, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
			xy <- cbind(xy[[1]], xy[[2]])
			to[cells] <- .xyValues(from, xy, method=method)
			return(to)
			
		} else {
			tr <- blockSize(to)
			pb <- pbCreate(tr$n, ...)	
			to <- writeStart(to, filename=filename, ...)
			for (i in 1:tr$n) {
				cells <- cellFromRowCol(to, tr$row[i], 1):cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, ncol(to))
				xy <- xyFromCell(to, cells ) 
				xy <- subset(xy, xy[,1] > e@xmin & xy[,1] < e@xmax)
				if (nrow(xy) > 0) {
					ci <- match(cellFromXY(to, xy), cells)
					xy <- .Call("transform", projto, projfrom, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
					xy <- cbind(xy[[1]], xy[[2]])
					v <- matrix(nrow=length(cells), ncol=nl)
					v[ci, ] <- .xyValues(from, xy, method=method)
					to <- writeValues(to, v, tr$row[i])
				}	
				pbStep(pb)
			}
			pbClose(pb)
			to <- writeStop(to)	
			return(to)
		}
	}
}

