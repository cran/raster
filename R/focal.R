# Author: Robert J. Hijmans
# Date :  October 2011
# Version 1.0
# Licence GPL v3

focal <- function(x, w, fun, filename='', na.rm=FALSE, pad=FALSE, padValue=NA, NAonly=FALSE, ...) {

	stopifnot(nlayers(x)==1)
	stopifnot(hasValues(x))
	
	# mistakes because of differences with old focal and old focalFilter
	dots <- list(...)
	if (!is.null(dots$filter)) {
		if (missing(w)) {
			w <- dots$filter
			warning('argument "filter" is wrong; use "w" instead')
		} else {
			warning('argument "filter" is ignored!')
		}
	}
	if (!is.null(dots$ngb)) {
		if (missing(w)) {
			w <- dots$ngb
			warning('argument "ngb" is wrong; use "w" instead')
		} else {
			warning('argument "ngb" is ignored!')		
		}
	}
	
	if (missing(w)) {
		stop('argument "w" is missing')
	}
	if (length(w) == 1) {
		w <- round(w)
		stopifnot(w > 1)
		w=matrix(1, nc=w, nr=w)
	} else if (length(w) == 2) {
		w <- round(w)
		w=matrix(1, nc=w[1], nr=w[2])
	} 
	if (! is.matrix(w) ) {
		stop('w should be a single number, two numbers, or a matrix')
	} 
	d <- dim(w)
	if (prod(d) == 0) { stop('ncol and nrow of w must be > 0') }
	if (min(d %% 2) == 0) { stop('w must have uneven sides') }	
	
	
	# to get the weights in the (by row) order for the C routine
	# but keeping nrow and ncol as-is
	w[] <- as.vector(t(w))
		
	out <- raster(x)
	filename <- trim(filename)
	
	padrows <- FALSE
	if (pad) {
		padrows <- TRUE
	}

	gll <- as.integer(.isGlobalLonLat(out))
	if (gll) {
		pad <- TRUE
	}

	
	if (missing(fun)) {
		dofun <- FALSE
	} else {
		dofun <- TRUE
		e <- new.env()
		oldfun <- fun
		if (na.rm) {
			fun <- function(x) as.double( oldfun(x, na.rm=TRUE) )
		} else {
			fun <- function(x) as.double( oldfun(x) )
		}
	}
	if (NAonly) {
		na.rm <- TRUE
	}
	NAonly <- as.integer(NAonly)
	narm <- as.integer(na.rm)
	
	if (canProcessInMemory(out)) {
		if (pad) {
			# this should be done in C, but for now....
			f <- floor(d / 2)
			v <- as.matrix(x)
			if (padrows) {
				padRows <- matrix(padValue, ncol=ncol(out), nrow=f[1])
				v <- rbind(padRows, v, padRows)
			} 
			if (gll) {
				v <- cbind(v[, (ncol(v)-f[2]+1):ncol(v)], v, v[, 1:f[2]])	
			} else {
				padCols <- matrix(padValue, nrow=nrow(v), ncol=f[2])
				v <- cbind(padCols, v, padCols)
			}
			
			paddim <- as.integer(dim(v))
			if (dofun) {
				v <- .Call('focal_fun', as.vector(t(v)), w, paddim, fun, NAonly, e, NAOK=TRUE, PACKAGE='raster')
			} else {
				v <- .Call('focal_sum', as.vector(t(v)), w, paddim, narm, NAonly, NAOK=TRUE, PACKAGE='raster')
			}
			v <- matrix(v, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
			if (padrows) {
				v <- v[-c(1:f[1], (nrow(v)-f[1]+1):nrow(v)), -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 
			} else {
				v <- v[, -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 			
			}
			v <- as.vector(t(v))
			
		} else {
		
			if (dofun) {
				v <- .Call('focal_fun', values(x), w, as.integer(dim(out)), fun, NAonly, e, NAOK=TRUE, PACKAGE='raster')
			} else {
				v <- .Call('focal_sum', values(x), w, as.integer(dim(out)), narm, NAonly, NAOK=TRUE, PACKAGE='raster')
			}
		}
		
		out <- setValues(out, v)
		if (filename  != '') {
			out <- writeRaster(out, filename, ...)
		}
		
	} else {

		out <- writeStart(out, filename,...)
		tr <- blockSize(out, minblocks=3, minrows=3)
		pb <- pbCreate(tr$n, type=.progress(...))

		addr <- floor(nrow(w) / 2)
		addc <- floor(ncol(w) / 2)
		nc <- ncol(out)
		nc1 <- 1:(nc * addc)
		
		if (pad) {
			f <- floor(d / 2)
			v <- getValues(x, row=1, nrows=tr$nrows[1]+addr)
			v <- matrix(v, ncol=ncol(out), byrow=TRUE)
			if (padrows) {
				padRows <- matrix(padValue, ncol=ncol(out), nrow=f[1])
				v <- rbind(padRows, v, padRows)
			}
			if (gll) {
				v <- cbind(v[, (ncol(v)-f[2]+1):ncol(v)], v, v[, 1:f[2]])			
			} else {
				padCols <- matrix(padValue, nrow=tr$nrows[1]+addr+2*f[1], ncol=f[2])
				v <- cbind(padCols, v, padCols)
			}
			paddim <- as.integer(dim(v))

			if (dofun) {
				v <- .Call('focal_fun', as.vector(t(v)), w, paddim, fun, NAonly, e, NAOK=TRUE, PACKAGE='raster')
			} else {
				v <- .Call('focal_sum', as.vector(t(v)), w, paddim, narm, NAonly, NAOK=TRUE, PACKAGE='raster')
			}
			v <- matrix(v, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
			if (padrows) {
				v <- v[-c(1:f[1], (nrow(v)-f[1]+1):nrow(v)), -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 
			} else {
				v <- v[ , -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 			
			}
			v <- as.vector(t(v))
			out <- writeValues(out, v, 1)
			pbStep(pb)
			
			for (i in 2:(tr$n-1)) {
				v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+(2*addr))
				v <- matrix(v, ncol=ncol(out), byrow=TRUE)
				if (padrows) {
					padRows <- matrix(padValue, ncol=ncol(out), nrow=f[1])
					v <- rbind(padRows, v, padRows)
				}
				if (gll) {
					v <- cbind(v[, (ncol(v)-f[2]+1):ncol(v)], v, v[, 1:f[2]])			
				} else {
					padCols <- matrix(padValue, nrow=tr$nrows[i]+addr+2*f[1], ncol=f[2])
					v <- cbind(padCols, v, padCols)
				}
				paddim <- as.integer(dim(v))
				if (dofun) {
					v <- .Call('focal_fun', as.vector(t(v)), w, paddim, fun, NAonly, e, NAOK=TRUE, PACKAGE='raster')
				} else {
					v <- .Call('focal_sum', as.vector(t(v)), w, paddim, narm, NAonly, NAOK=TRUE, PACKAGE='raster')
				}
				v <- matrix(v, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
				if (padrows) {
					v <- v[-c(1:f[1], (nrow(v)-f[1]+1):nrow(v)), -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 
				} else {
					v <- v[, -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 				
				}
				v <- as.vector(t(v))
				out <- writeValues(out, v[-nc1], tr$row[i])
				pbStep(pb) 
			}
			i <- tr$n
			v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+addr)
			v <- matrix(v, ncol=ncol(out), byrow=TRUE)
			if (padrows) {
				padRows <- matrix(padValue, ncol=ncol(out), nrow=f[1])
				v <- rbind(padRows, v, padRows)
			}
			if (gll) {
				v <- cbind(v[, (ncol(v)-f[2]+1):ncol(v)], v, v[, 1:f[2]])			
			} else {
				padCols <- matrix(padValue, nrow=tr$nrows[i]+addr+2*f[1], ncol=f[2])
				v <- cbind(padCols, v, padCols)
			}
			paddim <- as.integer(dim(v))

			if (dofun) {
				v <- .Call('focal_fun', as.vector(t(v)), w, paddim, fun, NAonly, e, NAOK=TRUE, PACKAGE='raster')
			} else {
				v <- .Call('focal_sum', as.vector(t(v)), w, paddim, narm, NAonly, NAOK=TRUE, PACKAGE='raster')
			}
			v <- matrix(v, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
			if (padrows) {
				v <- v[-c(1:f[1], (nrow(v)-f[1]+1):nrow(v)), -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 
			} else {
				v <- v[, -c(1:f[2], (ncol(v)-f[2]+1):ncol(v))] 				
			}
			v <- as.vector(t(v))
			
			out <- writeValues(out, v[-nc1], tr$row[i])
			pbStep(pb) 
		
		} else {
		
			v <- getValues(x, row=1, nrows=tr$nrows[1]+addr)
			if (dofun) {
				v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[1]+addr, nc)), fun, NAonly, e, NAOK=TRUE, PACKAGE='raster')
			} else {
				v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[1]+addr, nc)), narm, NAonly, NAOK=TRUE, PACKAGE='raster')
			}
			out <- writeValues(out, v, 1)
			pbStep(pb)
			for (i in 2:(tr$n-1)) {
				v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+(2*addr))
				if (dofun) {
					v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[i]+(2*addr), nc)), fun, NAonly, e, NAOK=TRUE, PACKAGE='raster')
				} else {
					v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[i]+(2*addr), nc)), narm, NAonly, NAOK=TRUE, PACKAGE='raster')
				}
				out <- writeValues(out, v[-nc1], tr$row[i])
				pbStep(pb) 
			}
			i <- tr$n
			v <- getValues(x, row=tr$row[i]-addr, nrows=tr$nrows[i]+addr)
			if (dofun) {
				v <- .Call('focal_fun', v, w, as.integer(c(tr$nrows[i]+addr, nc)), fun, NAonly, e, NAOK=TRUE, PACKAGE='raster')
			} else {
				v <- .Call('focal_sum', v, w, as.integer(c(tr$nrows[i]+addr, nc)), narm, NAonly, NAOK=TRUE, PACKAGE='raster')
			}
			out <- writeValues(out, v[-nc1], tr$row[i])
			pbStep(pb) 
		}
		out <- writeStop(out)			
		pbClose(pb)	
	}
	return(out)
}


