# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  Jaunary 2009
# Version 0.9
# Licence GPL v3


resample <- function(from, to, method, filename="", ...)  {
	
	# to do: compare projections of from and to
		
	if (nlayers(from) == 1) {
		to <- raster(to)
	} else {
		to <- brick(to, values=FALSE)
	}
	
	if (!hasValues(from)) {
		return(to)
	}	

	if (missing(method)) {
		stop("provide a method: 'bilinear' or 'ngb'")
	}
	if (!method %in% c('bilinear', 'ngb')) {
		stop('invalid method') 
	}
	if (method == 'ngb') method <- 'simple'
	
	filename <- trim(filename)

	resdif <- max(res(to) / res(from))
	if (resdif > 3) {
		warning('you are resampling to a raster with a much larger cell size, perhaps you should use "aggregate" first')
	}
	
	e = intersectExtent(from, to, validate=TRUE)
	
	if (is.null(filename)){ filename <- "" }
	
	if (!canProcessInMemory(to, 3) && filename == '') {
		filename <- rasterTmpFile()	
	}
	
	inMemory <- filename == ""
	if (inMemory) {
		v <- matrix(NA, nrow=ncell(to), nlayers(from))
	} else {
		to <- writeStart(to, filename=filename, ... )
	}


	if (.doCluster()) {
	
		cl <- .getCluster()
		on.exit( .returnCluster(cl) )
		
		nodes <- min(ceiling(to@nrows/10), length(cl)) # at least 10 rows per node
		
		cat('Using cluster with', nodes, 'nodes\n')
		flush.console()
		
		tr <- blockSize(to, minblocks=nodes)
		pb <- pbCreate(tr$n, type=.progress(...))

		clFun <- function(i) {
			r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
			xy <- xyFromCell(to, cellFromRowCol(to, tr$row[i], 1) : cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, ncol(to)) ) 
			raster:::.xyValues(from, xy, method=method)
		}
		
        for (i in 1:nodes) {
			sendCall(cl[[i]], clFun, i, tag=i)
		}

		if (inMemory) {
			for (i in 1:tr$n) {
				d <- recvOneData(cl)
				if (! d$value$success) {
					stop('cluster error')
				}
				start <- cellFromRowCol(to, tr$row[d$value$tag], 1)
				end <- cellFromRowCol(to, tr$row[d$value$tag]+tr$nrows[d$value$tag]-1, to@ncols)
				v[start:end, ] <- d$value$value

				if ((nodes + i) <= tr$n) {
					sendCall(cl[[d$node]], clFun, i, tag=i)
				}
				pbStep(pb)
			}
			to <- setValues(to, v)
			
		} else {
		
			for (i in 1:tr$n) {
				d <- recvOneData(cl)
				to <- writeValues(to, d$value$value, tr$row[d$value$tag])
				if ((nodes + i) <= tr$n) {
					sendCall(cl[[d$node]], clFun, nodes+i, tag=i)
				}
				pbStep(pb)
			}
			to <- writeStop(to)	
		}	
		
	} else {
		tr <- blockSize(to)
		pb <- pbCreate(tr$n, type=.progress(...))
		
		if (inMemory) {
			for (i in 1:tr$n) {
				r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
				xy <- xyFromCell(to, cellFromRowCol(to, tr$row[i], 1) : cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, ncol(to)) ) 
				vals <- .xyValues(from, xy, method=method)

				start <- cellFromRowCol(to, tr$row[i], 1)
				end <- cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, to@ncols)
				v[start:end, ] <- vals

				pbStep(pb, i)
			}
			to <- setValues(to, v)
		} else {
			for (i in 1:tr$n) {
				r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
				xy <- xyFromCell(to, cellFromRowCol(to, tr$row[i], 1) : cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, ncol(to)) ) 
				vals <- .xyValues(from, xy, method=method)
	
				to <- writeValues(to, vals, tr$row[i])

				pbStep(pb, i)
			}
			to <- writeStop(to)	
		}
	}

	pbClose(pb)
	return(to)
	
}

