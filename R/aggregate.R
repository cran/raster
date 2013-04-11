# Author: Robert J. Hijmans
# Date : July 2010
# Version 1.0
# Licence GPL v3

# October 2012: Major overhaul (including C interface)
# November 2012: fixed bug with expand=F


setMethod('aggregate', signature(x='Raster'), 
function(x, fact=2, fun='mean', expand=TRUE, na.rm=TRUE, filename="", ...)  {

	doC <- list(...)$doC
	if (is.null(doC)) doC <- TRUE
	fact <- rep(as.integer(round(fact)), length.out=2)
	xfact <- fact[1]
	yfact <- fact[2]
	if (xfact < 1 | yfact < 1) { stop('fact should be > 0') } 
	if (xfact < 2 & yfact < 2) { stop('fact[1] or fact[2] should be > 1') } 
	
	if (xfact > ncol(x)) {
		warning('aggregation factor is larger than the number of columns') 
		xfact <- ncol(x)
	}
	if (yfact > nrow(x)) {
		warning('aggregation factor is larger than the number of rows')
		yfact <- nrow(x)
	}

	ncx <- ncol(x)
	nrx <- nrow(x)
	if (expand) {
		rsteps <- as.integer(ceiling(nrx/yfact))
		csteps <- as.integer(ceiling(ncx/xfact))
		lastcol <- x@ncols
		lastrow <- x@nrows
		#addcols <- csteps * xfact - ncx
		#addrows <- rsteps * yfact - nrx

	} else 	{
		rsteps <- as.integer(floor(nrx/yfact))
		csteps <- as.integer(floor(ncx/xfact))
		lastcol <- min(csteps * xfact, x@ncols)
		lastrow <- min(rsteps * yfact, x@nrows)
	}
	
	
	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
		
	nl <- nlayers(x)
	if (nl > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)		
	}
	extent(out) <- extent(xmin(x), xmx, ymn, ymax(x))
	dim(out) <- c(rsteps, csteps) 
	names(out) <- names(x)
	ncout <- ncol(out)

	if (! hasValues(x) ) {	return(out) }	

	fun <- raster:::.makeTextFun(fun)
	if (class(fun) == 'character') { 
		op <- as.integer(match(fun, c('sum', 'mean', 'min', 'max')) - 1)
	} else {
		op <- NA
	}
	
	if (!is.na(op) & doC) {
	
		if ( canProcessInMemory(x)) {
		
			dims <- as.integer(c(lastrow, lastcol, nl, dim(out)[1:2], xfact, yfact))
			x <- getValuesBlock(x, 1, lastrow, 1, lastcol)
			out <- setValues(out, .Call("aggregate", as.double(x), op, as.integer(na.rm), dims, PACKAGE='raster'))

			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
			
		} else {
		
			out <- writeStart(out, filename=filename, ...)
			tr <- blockSize(x, minrows=yfact)

			st <- round(tr$nrows[1] / yfact) * yfact
			tr$n <- ceiling(lastrow / st)
			tr$row <- c(1, cumsum(rep(st, tr$n-1))+1)
			tr$nrows <- rep(st, tr$n)
			tr$write <- cumsum(c(1, ceiling(tr$nrows[1:(tr$n-1)]/yfact)))
			dif <- sum(tr$nrows) - nrow(x)
			if (dif > 0) {
				if (expand) {
					tr$nrows[tr$n] <-  tr$nrows[tr$n] - dif
				} else {
					dif <- dif %/% xfact
					if (dif > 0) {
						tr$nrows[tr$n] <- dif * xfact
					} else {
						tr$n <- tr$n - 1
					}
				}
			}
			tr$outrows <- ceiling(tr$nrows/yfact)
			
			pb <- pbCreate(tr$n, label='aggregate', ...)

			dims <- as.integer(c(lastrow, lastcol, nl, dim(out)[1:2], xfact, yfact))
			if (inherits(out, 'RasterBrick')) {
				for (i in 1:tr$n) {
					dims[c(1, 4)] = as.integer(c(tr$nrows[i], tr$outrows[i]))
					vals <- getValuesBlock(x, tr$row[i], tr$nrows[i], 1, lastcol)
					vals <- .Call("aggregate", as.double(vals), op, as.integer(na.rm), dims, PACKAGE='raster')
					out <- writeValues(out, matrix(vals, ncol=nl), tr$write[i])
					pbStep(pb, i) 
				}
			} else {
				for (i in 1:tr$n) {
					dims[c(1, 4)] = as.integer(c(tr$nrows[i], tr$outrows[i]))
					vals <- getValuesBlock(x, tr$row[i], tr$nrows[i], 1, lastcol)
					vals <- .Call("aggregate", as.double(vals), op,	as.integer(na.rm), dims, PACKAGE='raster')
					out <- writeValues(out, vals, tr$write[i])
					pbStep(pb, i) 
				}
			}
			pbClose(pb)
			out <- writeStop(out)
			return(out)	
		}
	}
	
 # else not implemented in C  
	
	if (nl < 2) {	

		if (class(fun) == 'character') { 
			rowcalc <- TRUE 
			fun <- raster:::.getColFun(fun)
		} else { 
			rowcalc <- FALSE 
		}
	
		if ( canProcessInMemory(x)) {
			if (expand) {
				m <- ceiling(nrx / yfact)
			} else {
				m <- floor(nrx / yfact)
			}

			vv <- matrix(NA, nrow= yfact*xfact, ncol=csteps * m)
			vend <- 0
			vvstart <- 1
			
			if (expand) {
				vals <- getValues(x)
				yf <- nrx %% yfact
			} else {
				vals <- getValuesBlock(x, 1, lastrow, 1, lastcol)
				yf <- 0
			}
			for (j in 1:m) {
				if (j == m & yf > 0) {
					vstart <- vend + 1
					vend <- vend + (lastcol * yf)
					mv <- matrix(vals[vstart:vend], nrow=yf, byrow=TRUE )
					temp <- matrix(nrow=yf*xfact, ncol=csteps)
					temp[1:length(mv)] <- mv
					cols <- 1:(csteps) + (m-1) * csteps
					vv[1:nrow(temp), cols] <- temp
							
				} else {
					vstart <- vend + 1
					vend <- vend + (lastcol * yfact)
					mv <- matrix(vals[vstart:vend], nrow=yfact, byrow=TRUE )
							
					vv[vvstart:(vvstart+length(mv)-1)] <- as.vector(mv)
					vvstart <- vvstart + ncout*nrow(vv)
				}
			}
			if (rowcalc) {
				vals <- fun(vv, na.rm=na.rm )
			} else {
				vals <- apply(vv, 2, fun, na.rm=na.rm )
			}
			out <- setValues(out, as.vector(vals))
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
		
		} else {
		
			out <- writeStart(out, filename=filename, ...)
			
			
			tr <- blockSize(x, minrows=yfact)
			st <- round(tr$nrows[1] / yfact) * yfact
			tr$n <- ceiling(lastrow / st)
			tr$row <- c(1, cumsum(rep(st, tr$n-1))+1)
			tr$nrows <- rep(st, tr$n)
			tr$write <- cumsum(c(1, ceiling(tr$nrows[1:(tr$n-1)]/yfact)))
			dif <- sum(tr$nrows) - nrow(x)
			if (dif > 0) {
				if (expand) {
					tr$nrows[tr$n] <-  tr$nrows[tr$n] - dif
				} else {
					dif <- dif %/% xfact
					if (dif > 0) {
						tr$nrows[tr$n] <- dif * xfact
					} else {
						tr$n <- tr$n - 1
					}
				}
			}
			
			pb <- pbCreate(tr$n, label='aggregate', ...)
			m <- tr$nrows[1] / yfact
			vv <- matrix(NA, nrow= yfact*xfact, ncol=csteps * m)
		
			w <- getOption('warn')
			on.exit(options('warn' = w))
			options('warn'=-1) 
		
			for (i in 1:(tr$n-1)) {
				vals <- getValuesBlock(x, tr$row[i], tr$nrows[i], 1, lastcol)		
				vend <- 0
				vvstart <- 1
				for (j in 1:m) {
					vstart <- vend + 1
					vend <- vend + (lastcol * yfact)
					mv <- matrix(vals[vstart:vend], nrow=yfact, byrow=TRUE )
						
					vv[vvstart:(vvstart+length(mv)-1)] <- as.vector(mv)			
					vvstart <- vvstart + ncout*nrow(vv)
				}
				if (rowcalc) {
					vals <- fun(vv, na.rm=na.rm )
				} else {
					vals <- apply(vv, 2, fun, na.rm=na.rm )
				}
				out <- writeValues(out, vals, tr$write[i])
				pbStep(pb, i) 
			} 

	#	if (i==tr$n) { 
			i <- tr$n
			vals <- getValuesBlock(x, tr$row[i], tr$nrows[i], 1, lastcol)		
			m <- ceiling(tr$nrows[i] / yfact)
			vv <- matrix(NA, nrow= yfact*xfact, ncol=csteps * m)
			vend <- 0
			vvstart <- 1
			yf <- tr$nrows[i] %% yfact
			for (j in 1:m) {
				if (j == m & yf > 0) {
					vstart <- vend + 1
					vend <- vend + (lastcol * yf)
					mv <- matrix(vals[vstart:vend], nrow=yf, byrow=TRUE )
					temp <- matrix(nrow=yf*xfact, ncol=csteps)
					temp[1:length(mv)] <- mv
					cols <- 1:(csteps) + (m-1) * csteps
					vv[1:nrow(temp), cols] <- temp
					
				} else {
					vstart <- vend + 1
					vend <- vend + (lastcol * yfact)
					mv <- matrix(vals[vstart:vend], nrow=yfact, byrow=TRUE )
					
					vv[vvstart:(vvstart+length(mv)-1)] <- as.vector(mv)
					vvstart <- vvstart + ncout*nrow(vv)
				}
			}
			if (rowcalc) {
				vals <- fun(vv, na.rm=na.rm )
			} else {
				vals <- apply(vv, 2, fun, na.rm=na.rm )
			}
			pbStep(pb, i) 
			out <- writeValues(out, vals, tr$write[i])
			pbClose(pb)
			out <- writeStop(out)
			return(out)
		}
		
	} else { # nlayers > 1
	
		if (canProcessInMemory(x, nlayers(x)+2)) {
			
			if (class(fun) == 'character') { 
				op <- as.integer(match(fun, c('sum', 'mean', 'min', 'max')) - 1)
			}
			if (!is.na(op) & doC) {
				dim <- c(dim(x), dim(out)[1:2], xfact, yfact)
				v  <- .Call("aggregate", 
						as.double(getValues(x)), op, as.integer(na.rm), 
						as.integer(dim), PACKAGE='raster')
						
				out <- setValues(out, matrix(v, ncol=dim[3]))		
				return(out)	
			}
			

			xx <- raster(x)		
			x <- getValues(x)
			cols <- rep(rep(1:csteps, each=xfact)[1:ncol(xx)], times=nrow(xx))
			rows <- rep(1:rsteps, each=ncol(xx) * yfact)[1:ncell(xx)]
			cells <- cellFromRowCol(xx, rows, cols)
						
			x <- as.matrix( aggregate(x, list(cells), fun, na.rm=na.rm ))[,-1]
			rm(cells)
			
			x <- setValues(out, x)
			if (filename != "") {
				x <- writeRaster(x, filename=filename, ...)
			}
			return(x)

		} else  { 
		
			cols <- rep(rep(1:csteps,each=xfact)[1:ncol(x)], times=yfact)
			rows <- rep(1, each=(ncol(x) * yfact))
			
			out <- writeStart(out, filename=filename, ...)
			
			cells <- cellFromRowCol(x, rows, cols)
			nrows = yfact

			w <- getOption('warn')
			on.exit(options('warn' = w))
			options('warn'=-1) 
			
			pb <- pbCreate(rsteps, label='aggregate', ...)
			for (r in 1:rsteps) {
				startrow <- 1 + (r - 1) * yfact
				if ( r==rsteps) {
					endrow <- min(nrow(x), startrow + yfact - 1)
					nrows <- endrow - startrow + 1
					theserows <- (startrow * rows)[1:(ncol(x)*nrows)]
					cols <- cols[1:(ncol(x)*nrows)]
					cells <- cellFromRowCol(x, theserows, cols)
				}	
				vals <- getValues(x, startrow, nrows)
				vals <- as.matrix( aggregate(vals, list(cells), fun, na.rm=na.rm ))[,-1]
			
				out <- writeValues(out, vals, r)
				pbStep(pb, r) 
			} 
			pbClose(pb)
			out <- writeStop(out)
			return(out)
		}	
	}
}
)



#library(raster)
#r <- raster(nc=9, nr=9)
#r <- raster()
#r[] = 1:ncell(r)
#raster:::.aggtest(r, 5, 'min', doC=T)
