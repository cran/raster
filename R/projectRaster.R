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
	
	obj <- raster(extent(minx, maxx, miny,  maxy), nrows=nrow(object), ncols=ncol(object), crs=crs)
	return(obj)
}


projectRaster <- function(from, to, method="bilinear", filename="", ...)  {
	if (! .requireRgdal() ) { stop('rgdal not available') }
	
	if (! inherits(from, 'RasterStack' )) {
		if ( ! inMemory(from) & ! fromDisk(from) ) {
			stop('no vales for "from". Nothing to do.')
		}
	}

	validObject(to)
	validObject(projection(from, asText=FALSE))
	validObject(projection(to, asText=FALSE))
	projfrom <- projection(from)
	projto <- projection(to)
	if (projfrom == "NA") { stop("input projection is NA") }
	if (projto == "NA") { stop("output projection is NA") }
	if (identical(projfrom, projto)) {
		if ( identical( as(from, 'BasicRaster'), as(to, 'BasicRaster') ) ) {
			warning('nothing to do; returning "from"')
			return(from)
		} else {
			warning('projections are the same, using resample')
			return( resample(from, to, method=method, filename=filename, ...  ) ) 
		}
	}	
	
	pbb <- projectExtent(to, projection(from))
	bb <- intersectExtent(pbb, from)
	validObject(bb)

	if (!method %in% c('bilinear', 'ngb')) { stop('invalid method') }
	if (method=='ngb') { method <- 'simple' } # for xyValues

	filename <- trim(filename)
	
	if ( nlayers(from) == 1) {
		to <- raster(to)
	} else {
		to <- brick(to, values=FALSE)
		to@data@nlayers <- nlayers(from)
	}
	
	if (!canProcessInMemory(to, 1) && filename == "") {
		filename <- rasterTmpFile()
	}

	inMemory <- filename == ""
	if (inMemory) {
		v <- matrix(NA, nrow=ncell(to), nlayers(from))
	} else {
		to <- writeStart(to, filename=filename, ...)
	}
	
	tr <- blockSize(to)
	pb <- pbCreate(tr$n, type=.progress(...))
	for (i in 1:tr$n) {
		r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
		xy <- xyFromCell(to, cellFromRowCol(to, tr$row[i], 1) : cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, ncol(to)) ) 

		unProjXY <- .Call("transform", projto, projfrom, nrow(xy), xy[,1], xy[,2], PACKAGE="rgdal")
		unProjXY <- cbind(unProjXY[[1]], unProjXY[[2]])
		
		vals <- xyValues(from, unProjXY, method=method)
		
		if (inMemory) {
			start <- cellFromRowCol(to, tr$row[i], 1)
			end <- cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, to@ncols)
			v[start:end, ] <- vals
		} else {
			to <- writeValues(to, vals, tr$row[i])
		}
		pbStep(pb)
		
	}
	pbClose(pb)
	
	if (inMemory) {
		to <- setValues(to, v)
	} else {
		to <- writeStop(to)	
	}
	return(to)
}

