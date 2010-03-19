# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


projectExtent <- function(object, projs) {
	if (!require(rgdal)) { stop() }

	validObject(projection(object, asText=FALSE))
	validObject(projection(projs, asText=FALSE))
	projfrom <- projection(object)
	projto <- projection(projs)
	
	xy1 <- xyFromCell(object, cellFromCol(object, 1))
	xy1[,1] <- xy1[,1] - 0.5 * xres(object)
	xy1[1,2] <- xy1[1,2] + 0.5 * yres(object)
	xy1[nrow(object),2] <- xy1[nrow(object),2] + 0.5 * yres(object)
	
	xy2 <- xyFromCell(object, cellFromCol(object, ncol(object)))
	xy2[,1] <- xy2[,1] + 0.5 * xres(object)
	xy2[1,2] <- xy2[1,2] + 0.5 * yres(object)
	xy2[nrow(object),2] <- xy2[nrow(object),2] + 0.5 * yres(object)
	
	xy <- rbind(xy1, xy2)
	
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
	
	obj <- raster(extent(minx, maxx, miny,  maxy), nrows=nrow(object), ncols=ncol(object), proj=(projs))
	return(obj)
}


projectRaster <- function(from, to, method="ngb", filename="", ...)  {
	if (!require(rgdal)) { stop() }
	
	
	if (dataContent(from) != 'all' & dataSource(from) == 'ram') { stop('no vales for "from". Nothing to do') }

	validObject(to)
	validObject(projection(from, asText=FALSE))
	validObject(projection(to, asText=FALSE))
	projfrom <- projection(from)
	projto <- projection(to)
	if (projfrom == "NA") {stop("input projection is NA")}
	if (projto == "NA") {stop("output projection is NA")}
	
	pbb <- projectExtent(to, projection(from))
	bb <- intersectExtent(pbb, from)
	validObject(bb)

	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') }
	filename <- trim(filename)
	to <- raster(to, filename=filename)

	rowCells <- 1:ncol(to)

	if (!canProcessInMemory(to, 1) && filename == "") {
		filename <- rasterTmpFile()
		
	}
	inMemory <- filename == ""
	if (inMemory) {
		v <- matrix(NA, nrow=ncol(to), ncol=nrow(to))
	}
	
	if (method=='ngb') {
		xymethod <- 'simple' 
	} else {
		xymethod <- 'bilinear' 	
	}
	
	
	pb <- pbCreate(nrow(to), type=.progress(...))
	
	for (r in 1:nrow(to)) {
		cells <- rowCells + (r-1) * ncol(to)
		xy <- xyFromCell(to, cells)
		res <- .Call("transform", projto, projfrom, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
		unProjXY <- cbind(res[[1]], res[[2]])
		vals <- xyValues(from, unProjXY, method=xymethod)
		if (inMemory) {
			v[,r] <- vals
		} else {
			to <- setValues(to, vals, r)
			to <- writeRaster(to, filename=filename, ...)
		}
		pbStep(pb, r)
	}
	pbClose(pb)
	if (inMemory) {
		to <- setValues(to, as.vector(v))
	}
	return(to)
}


