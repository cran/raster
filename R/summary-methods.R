# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3

.addArgs <- function(...) {
	lst <- list(...)
	add <- list()
	if (length(lst) > 0 ) {
		cnt <- 0
		for (i in 1:length(lst)) {
		# is.atomic ?
			if (class(lst[[i]]) %in% c('logical', 'integer', 'numeric')) {
				cnt <- cnt + 1
				add[[cnt]] <- lst[[i]]
			}
		}
	}
	return(unlist(add))
}


setMethod("mean", signature(x='Raster'),
	function(x, ..., trim = 0, na.rm=FALSE){
		rasters <- .makeRasterList(x, ...)
		add <- .addArgs(...)
		fun <- function(...){ mean(..., trim=trim ) }
		return( .summaryRasters(rasters=rasters, add=add, fun=fun, na.rm=na.rm) )		
	}
)



setMethod("Summary", signature(x='Raster'),
	function(x, ..., na.rm=FALSE){
		call <- sys.call()
        fun <- as.character(call[[1L]])
		rasters <- .makeRasterList(x, ...)
		add <- .addArgs(...)
		rm(x)
		return( .summaryRasters(rasters=rasters, add=add, fun=fun, na.rm=na.rm) )
	}
)


		
.summaryRasters <- function(rasters, add, fun, na.rm=na.rm) {

#	fun = match.fun(fun)

	if (length(rasters)==1 & length(add)==0) {
		warning('nothing to summarize if you provide a single RasterLayer')
		return(rasters[[1]])
	}	
	
	r <- raster(rasters[[1]])
	tr <- blockSize(r, n=length(rasters))
	if (!canProcessInMemory(r, length(rasters)+1)) {
		filename <- rasterTmpFile()
		r <- writeStart(r, filename=filename, overwrite=TRUE )
	} else {
		filename <- ""
		v <- matrix(ncol=nrow(r), nrow=ncol(r))
	}

	m <- matrix(NA, nrow=tr$size * ncol(r), ncol=length(rasters))
	if (length(add) > 0) {
		m <- cbind(m, add) 
	}
	if (na.rm) {
		on.exit(options('warn'= getOption('warn')))
		options('warn'=-1)  # for NA rows in apply
	}
	pb <- pbCreate(tr$n, type=.progress())			
	for (i in 1:tr$n) {
		if (i==tr$n & i > 1) {  # the last one could be smaller
			m = NULL
			for (j in 1:length(rasters)) {
				m <- cbind(m, getValuesBlock(rasters[[j]], row=tr$row[i], nrows=tr$size))
			}				
			m <- cbind(m, add)
		} else {
			for (j in 1:length(rasters)) {
				m[,j] <- getValuesBlock(rasters[[j]], row=tr$row[i], nrows=tr$size)
			}
		}
		if (na.rm) {
			vv <- apply(m, 1, FUN=fun, na.rm=TRUE)
		} else {
			vv <- apply(m, 1, FUN=fun)
		}
		if (class(vv) == 'matrix')  { # range
			vv <- vv[2,] - vv[1,]
		}
		
		if (filename == "") {
			vv <- matrix(vv, nrow=ncol(r))
			cols <- tr$row[i]:(tr$row[i]+dim(vv)[2]-1)	
			v[,cols] <- vv
		} else {
			writeValues(r, vv, tr$row[i])
		}
		pbStep(pb, i) 
	} 
	pbClose(pb)			
	if (filename == "") {
		r <- setValues(r, as.vector(v))
	} else {
		r <- writeStop(r)
	}
	return(r)
}

