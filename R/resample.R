# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  Jaunary 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("resample")) {
	setGeneric("resample", function(x, y, ...)
		standardGeneric("resample"))
}


setMethod('resample', signature(x='Raster', y='Raster'), 
function(x, y, method="bilinear", filename="", ...)  {
	
	# y do: compare projections of x and y
		
	ln <- layerNames(x)
	nl <- nlayers(x)
	if (nl == 1) {
		y <- raster(y)
	} else {
		y <- brick(y, values=FALSE, nl=nl)
	}
	
	if (!hasValues(x)) {
		return(y)
	}	

	if (!method %in% c('bilinear', 'ngb')) {
		stop('invalid method') 
	}
	if (method == 'ngb') method <- 'simple'
	

	resdif <- max(res(y) / res(x))
	if (resdif > 3) {
		warning('you are resampling y a raster with a much larger cell size, perhaps you should use "aggregate" first')
	}
	
	e <- intersectExtent(x, y, validate=TRUE)
	
	filename <- trim(filename)
	if (canProcessInMemory(y, 3*nl)) {
		inMemory <- TRUE
		v <- matrix(NA, nrow=ncell(y), ncol=nlayers(x))
	} else {
		inMemory <- FALSE
		y <- writeStart(y, filename=filename, ... )
	}


	if (.doCluster()) {
	
		cl <- getCluster()
		on.exit( returnCluster() )
		
		nodes <- min(ceiling(y@nrows/10), length(cl)) # at least 10 rows per node
		
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()
		
		tr <- blockSize(y, minblocks=nodes)
		pb <- pbCreate(tr$n, ...)

		clFun <- function(i) {
			#r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
			xy <- xyFromCell(y, cellFromRowCol(y, tr$row[i], 1) : cellFromRowCol(y, tr$row[i]+tr$nrows[i]-1, ncol(y)) ) 
			.xyValues(x, xy, method=method)
		}
		
        for (ni in 1:nodes) {
			sendCall(cl[[ni]], clFun, ni, tag=ni)
		}

		if (inMemory) {
			for (i in 1:tr$n) {
				d <- recvOneData(cl)
				if (! d$value$success) {
					stop('cluster error')
				}
				start <- cellFromRowCol(y, tr$row[d$value$tag], 1)
				end <- cellFromRowCol(y, tr$row[d$value$tag]+tr$nrows[d$value$tag]-1, y@ncols)
				v[start:end, ] <- d$value$value

				ni <- ni + 1
				if (ni <= tr$n) {
					sendCall(cl[[d$node]], clFun, ni, tag=ni)
				}
				pbStep(pb)
			}
			y <- setValues(y, v)
			if (filename != '') {
				writeRaster(y, filename, ...)
			}
			
		} else {
		
			for (i in 1:tr$n) {
				d <- recvOneData(cl)
				y <- writeValues(y, d$value$value, tr$row[d$value$tag])
				ni <- ni + 1
				if (ni <= tr$n) {
					sendCall(cl[[d$node]], clFun, ni, tag=ni)
				}
				pbStep(pb)
			}
			y <- writeStop(y)	
		}	
		
	} else {
	
		tr <- blockSize(y)
		pb <- pbCreate(tr$n, ...)
		
		if (inMemory) {
			for (i in 1:tr$n) {
				#r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
				xy <- xyFromCell(y, cellFromRowCol(y, tr$row[i], 1) : cellFromRowCol(y, tr$row[i]+tr$nrows[i]-1, ncol(y)) ) 
				vals <- .xyValues(x, xy, method=method)

				start <- cellFromRowCol(y, tr$row[i], 1)
				end <- cellFromRowCol(y, tr$row[i]+tr$nrows[i]-1, y@ncols)
				v[start:end, ] <- vals

				pbStep(pb, i)
			}
			v <- setValues(y, v)
			if (filename != '') {
				writeRaster(v, filename, ...)
				
			}
			pbClose(pb)
			layerNames(v) <- ln
			return(v)
			
		} else {
			for (i in 1:tr$n) {
				#r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
				xy <- xyFromCell(y, cellFromRowCol(y, tr$row[i], 1) : cellFromRowCol(y, tr$row[i]+tr$nrows[i]-1, ncol(y)) ) 
				vals <- .xyValues(x, xy, method=method)
	
				y <- writeValues(y, vals, tr$row[i])

				pbStep(pb, i)
			}
			y <- writeStop(y)	
		}
	}

	pbClose(pb)
	layerNames(y) <- ln
	return(y)
	
}
)
