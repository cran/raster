# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  February 2009
# Version 0.9
# Licence GPL v3


# to do: if proj is latlon & between -180 to 180, then use cells from other side..
#	global <- .isGlobalLatLon(raster)
#	if (global) {}
	

.checkngb <- function(ngb) {
	ngb <- as.integer(round(ngb))
	if (length(ngb) == 1) {
		ngb <- c(ngb, ngb)
	} else if (length(ngb) > 2) {
		stop('ngb should be a single value or two values')
	}
	if (min(ngb) < 2) { stop("ngb should be 2 or larger") } 
	return(ngb)
}
	
	
focal <- function(x, ngb=3, fun=mean, na.rm=TRUE, filename="", ...) {

	ngb <- .checkngb(ngb)
	out <- raster(x)
	
	row1 = floor(ngb[1]/2)
	row2 = ngb[1]-(row1+1)
	nrows = ngb[1]
	col1 = floor(ngb[2]/2)
	col2 = ngb[2]-(col1+1)
	
	add1 = matrix(0, ncol=col1, nrow=ngb[1])
	add2 = matrix(0, ncol=col2, nrow=ngb[1])

	nrs = matrix(ncol=ncol(out), nrow=ngb[1])
	nrs[] = 1:length(nrs)
	nrs = cbind(add1, nrs, add2)
	
	idx = matrix(ncol=ncol(out), nrow=prod(ngb))
	cc = 1:ngb[2]
	for (c in 1:ncol(out)) {
		idx[,c] = nrs[,cc] 
		cc = cc + 1
	}
	id = as.vector(idx)
	id = cbind(rep(1:ncol(out), each=nrow(idx)), id)
	id = subset(id, id[,2]>0)
	

	filename <- trim(filename)
	inmem = TRUE
	if (!canProcessInMemory(out, 3) && filename == '') {
		filename <- rasterTmpFile()
		inmem = FALSE
								
	}
	if (inmem) {
		v = matrix(nrow=ncol(out), ncol=nrow(out))		
	} else {
		out <- writeStart(out, filename=filename, ...)
	}

	pb <- pbCreate(nrow(out), type=.progress(...))
	ngbdata = matrix(nrow=ngb[1], ncol=ncol(x))
	rr = 0
	for (r in 1:row1) {
		rr = rr + 1
		ngbdata[rr,] = getValues(x, rr)
	}
	
	on.exit(options('warn'= getOption('warn')))
	options('warn'=-1) 

	for (r in 1:nrow(out)) {	
		rr = rr + 1
		if (rr <= ngb[1]) {
			ngbdata[rr,] = getValues(x, rr)
			ids = matrix(as.vector(idx), nrow=ngb[1])
			ids = ids[1:rr, ]
			ids = matrix(as.vector(ids), ncol=ncol(out))
			ids = cbind(rep(1:ncol(out), each=nrow(ids)), as.vector(ids))
			ids = subset(ids, ids[,2]>0)
			vv = tapply(as.vector(ngbdata)[ids[,2]], ids[,1], fun, na.rm=na.rm)
			
		} else if (r <= (nrow(out)-row1)) {
			ngbdata[1:(ngb[1]-1), ] <- ngbdata[2:(ngb[1]), ]
			ngbdata[ngb[1],] = getValues(x, rr)
			vv = tapply(as.vector(ngbdata)[id[,2]], id[,1], fun, na.rm=na.rm)
			
		} else {
			ngbdata[1:(ngb[1]-1), ] <- ngbdata[2:(ngb[1]), ]
			ngbdata[nrows,] = NA
			nrows=nrows-1
			ids = matrix(as.vector(idx), nrow=ngb[1])
			ids = ids[1:nrows, ]
			ids = matrix(as.vector(ids), ncol=ncol(out))
			ids = cbind(rep(1:ncol(out), each=nrow(ids)), as.vector(ids))
			ids = subset(ids, ids[,2]>0)
			vv = tapply(as.vector(ngbdata)[ids[,2]], ids[,1], fun, na.rm=na.rm)
		}
		if (inmem) {
			v[,r] <- vv
		} else {
			out <- writeValues(out, as.vector(vv), r)
		}
		
		pbStep(pb, r)
	}

	pbClose(pb)

	if (inmem) { 
		out <- setValues(out, as.vector(v)) 
		if (filename != "") {
			out <- writeRaster(out, filename, ...)
		}
	} else {
		out <- writeStop(out)
	}
	return(out)
}
	
