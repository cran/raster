# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3



.focal <- function(x, ngb=3, fun=mean, na.rm=TRUE, filename="", ...) {

	stopifnot(nlayers(x) == 1)
	
	ngb <- .checkngb(ngb)
	out <- raster(x)
	
	row1 <- floor(ngb[1]/2)
	row2 <- ngb[1]-(row1+1)
	nrows <- ngb[1]
	col1 <- floor(ngb[2]/2)
	col2 <- ngb[2]-(col1+1)

	nrs <- matrix(ncol=ncol(out), nrow=ngb[1])
	nrs[] <- 1:length(nrs)
	
	if (.isGlobalLonLat(out)) {
		add1 <- nrs[,(ncol(nrs)-col1+1):ncol(nrs),drop=FALSE]
		add2 <- nrs[,1:col2,drop=FALSE]
	} else {
		add1 <- matrix(0, ncol=col1, nrow=ngb[1])
		add2 <-matrix(0, ncol=col2, nrow=ngb[1])
	}

	nrs <- cbind(add1, nrs, add2)
	
	idx <- matrix(ncol=ncol(out), nrow=prod(ngb))
	cc <- 1:ngb[2]
	for (c in 1:ncol(out)) {
		idx[,c] <- nrs[,cc] 
		cc <- cc + 1
	}
	id <- as.vector(idx)
	id <- cbind(rep(1:ncol(out), each=nrow(idx)), id)
	id <- subset(id, id[,2]>0)
	

	filename <- trim(filename)
	if (canProcessInMemory(out, 4)) {
		inMem <- TRUE
		if (!inMemory(x)) {
			x <- readAll(x)
		}
		v <- matrix(nrow=ncol(out), ncol=nrow(out))		
	
	} else {
		inMem <- FALSE				
		if (filename == '') {
			filename <- rasterTmpFile()
		}
		out <- writeStart(out, filename=filename, ...)
	}

	pb <- pbCreate(nrow(out), ...)
	ngbdata <- matrix(nrow=ngb[1], ncol=ncol(x))
	rr <- 0
	for (r in 1:row1) {
		rr <- rr + 1
		ngbdata[rr,] <- getValues(x, rr)
	}
	w <- getOption('warn')
	on.exit(options('warn'= w))
	options('warn'=-1) 

	a <- 1:(ngb[1]-1)
	b <- 2:ngb[1]
	i <- ngb[1]
	for (r in 1:nrow(out)) {	
		rr <- rr + 1
		if (rr <= ngb[1]) {
			ngbdata[rr,] = getValues(x, rr)
			ids <- matrix(as.vector(idx), nrow=ngb[1])
			ids <- ids[1:rr, ]
			ids <- matrix(as.vector(ids), ncol=ncol(out))
			ids <- cbind(rep(1:ncol(out), each=nrow(ids)), as.vector(ids))
			ids <- subset(ids, ids[,2]>0)
			vv  <- tapply(as.vector(ngbdata)[ids[,2]], ids[,1], fun, na.rm=na.rm)
			
		} else if (r <= (nrow(out)-row1)) {
			ngbdata[a, ] <- ngbdata[b, ]
			ngbdata[i,] <- getValues(x, rr)
			vv <- tapply(as.vector(ngbdata)[id[,2]], id[,1], fun, na.rm=na.rm)
			
		} else {
			ngbdata[a, ] <- ngbdata[b, ]
			ngbdata[nrows,] <- NA
			nrows <- nrows-1
			ids <- matrix(as.vector(idx), nrow=ngb[1])
			ids <- ids[1:nrows, ]
			ids <- matrix(as.vector(ids), ncol=ncol(out))
			ids <- cbind(rep(1:ncol(out), each=nrow(ids)), as.vector(ids))
			ids <- subset(ids, ids[,2]>0)
			vv  <- tapply(as.vector(ngbdata)[ids[,2]], ids[,1], fun, na.rm=na.rm)
		}
		if (inMem) {
			v[,r] <- vv
		} else {
			out <- writeValues(out, as.vector(vv), r)
		}
		
		pbStep(pb, r)
	}

	pbClose(pb)

	if (inMem) { 
		out <- setValues(out, as.vector(v)) 
		if (filename != "") {
			out <- writeRaster(out, filename, ...)
		}
	} else {
		out <- writeStop(out)
	}
	return(out)
}
	
