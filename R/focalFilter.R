# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 1.0
# Licence GPL v3
#
# new version April 2011
# padding, wrapping of global lon/lat data

focalFilter <- function(x, filter, fun=sum, filename="", na.rm=FALSE, pad=TRUE, padValue=NA, ...) {

	warning('this function is depracated. Please use "focal"')

	stopifnot(nlayers(x) == 1)
	stopifnot(is.matrix(filter))
	
	ngb <- dim(filter)
	if (prod(ngb) == 0) { stop('ncol and nrow of filter must be > 0') }
	if (min(ngb%%2) == 0) { stop('filter must have uneven sides') }	

	out <- raster(x)
	
	limcol <- floor(ngb[2] / 2)
	colnrs <- (-limcol+1):(ncol(out)+limcol)
	colnrs <- .embed(colnrs, ngb[2]) + limcol 

	glob <- .isGlobalLonLat(x)
	notGlobPad <- !glob & !pad & na.rm
	notPad <- !pad & na.rm
	if (notGlobPad) {
		padNA <- c(1:limcol, (ncol(out)-limcol+1):ncol(out))
	}

	if (glob) {
		padfc <- 1:limcol
		fc <- padfc + limcol
		lc <- (ncol(out)+1):(ncol(out)+limcol)
		padlc <- lc + limcol
		padfclc <- c(padfc, padlc)
		lcfc <- c(lc, fc)
	}
	
	limrow <- floor(ngb[1] / 2)
	rdata <- matrix(padValue, ncol=ncol(x)+2*limcol, nrow=nrow(filter), byrow=TRUE) 
	colrange <- (limcol+1):(ncol(rdata)-limcol)

	fr <- (nrow(rdata)-limrow+1):nrow(rdata)
	rdata[fr, colrange] <- matrix(getValues(x, 1, limrow), nrow=limrow, byrow=TRUE)


	if (glob) {
		rdata[fr, padfc] <- rdata[fr, lc]
		rdata[fr, padlc] <- rdata[fr, fc]				
	}
	
	res <- rep(NA, ncol(out))
	
	filename <- trim(filename)
	if ( canProcessInMemory(out, 4) ) {
		inMem <- TRUE
		if (!inMemory(x)) {
			x <- readAll(x)
		}
		v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
	} else {
		inMem <- FALSE
		if (filename == '') {
			filename <- rasterTmpFile()			
		}
		out <- writeStart(out, filename=filename, ...)
	}
	
	pb <- pbCreate(nrow(out), type=.progress(...))

	lastrow <- nrow(filter)
	rrows <- 1:(lastrow-1)
	
	for (r in 1:nrow(out)) {
		rr <- r + limrow
		if (rr <= nrow(out)) {
			rdata[rrows,] <- rdata[rrows+1,]
			rdata[lastrow, colrange] <- getValues(x, rr)
			if (glob) {
				rdata[lastrow, padfclc] <- rdata[lastrow, lcfc]
#				rdata[lastrow, padfc] <- rdata[lastrow, lc]
#				rdata[lastrow, padlc] <- rdata[lastrow, fc]				
			}
		} else {
			rdata[rrows,] <- rdata[rrows+1,]
			rdata[lastrow, ] <- padValue
		}
		
		d <- matrix(as.vector(rdata[, t(colnrs)]), nrow=length(filter)) * as.vector(filter)
		vals <- apply(d, 2, FUN=fun, na.rm=na.rm)

		if (inMem) {
			v[,r] <- vals		
		} else {
			if (notGlobPad) {
				vals[padNA] <- NA
			}
			out <- writeValues(out, vals, r)
		}
		pbStep(pb, r)
	}
  	pbClose(pb)
	
	if (inMem) { 
		if( notPad ) {
			v[ , c(1:limrow, (nrow(out)-limrow+1):nrow(out)) ] <- NA
			if (!glob) {
				v[padNA, ] <- NA
			}
		}
		out <- setValues(out, as.vector(v)) 
		if (filename != "") {
			out <- writeRaster(out, filename, ...)
		}
	} else {
		if( notPad ) {
			out <- writeValues(out, rep(NA, ncol(out)*limrow), 1)
			out <- writeValues(out, rep(NA, ncol(out)*limrow), nrow(out)-limrow)
		}
		out <- writeStop(out)
	}
	return(out)
}

#q = focalFilter1(x, f)
#plot(q)
#qq = focalFilter1(x, f, na.rm=T, pad=F)
#plot(qq)

