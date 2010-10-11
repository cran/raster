# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : February 2010
# Version 0.9
# Licence GPL v3


if (!isGeneric("edge")) {
	setGeneric("edge", function(x, ...)
		standardGeneric("edge"))
}	


setMethod('edge', signature(x='RasterLayer'), 
function(x, filename="", classes=TRUE, type='both', asNA=FALSE, asZero=TRUE, ...) {
	
	ngb <- c(3,3)
	type = tolower(type)
	if (! type %in% c('both', 'inner', 'outer')) stop("type must be 'both', 'inner', or 'outer'")
	
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

	fun = function(x) length(unique(x)) != 1

	pb <- pbCreate(nrow(out), type=.progress(...))
	ngbdata = matrix(nrow=ngb[1], ncol=ncol(x))
	rr = 0
	for (r in 1:row1) {
		rr = rr + 1
		d = getValues(x, rr)
		if (! classes) {
			d[!is.na(d)] <- 1
		} else {
			d = round(d)
		}
		ngbdata[rr,] <- d
	}
	for (r in 1:nrow(out)) {	
		rr = rr + 1
		if (rr <= ngb[1]) {
			d = getValues(x, rr)
			if (! classes) {
				d[!is.na(d)] <- 1
			} else {
				d = round(d)
			}
			ngbdata[rr,] <- d
			ids = matrix(as.vector(idx), nrow=ngb[1])
			ids = ids[1:rr, ]
			ids = matrix(as.vector(ids), ncol=ncol(out))
			ids = cbind(rep(1:ncol(out), each=nrow(ids)), as.vector(ids))
			ids = subset(ids, ids[,2]>0)
			vv = as.vector(tapply(as.vector(ngbdata)[ids[,2]], ids[,1], fun))
			if (type == 'inner') {
				vv[is.na(ngbdata[1,])] = 0
			} else if (type == 'outer') {
				vv[! is.na(ngbdata[1,])] = 0
			}
			if (asNA) {		
				if (asZero) {
					vv[vv==0 & is.na(ngbdata[1,])] = NA 	
				} else {
					vv[vv==0] = NA 					
				}
			}
			
			
		} else if (r <= (nrow(out)-row1)) {
			ngbdata[1:(ngb[1]-1), ] <- ngbdata[2:(ngb[1]), ]
			d = getValues(x, rr)
			if (! classes) {
				d[!is.na(d)] <- 1
			} else {
				d = round(d)
			}
			ngbdata[ngb[1],] <- d
			vv = as.vector(tapply(as.vector(ngbdata)[id[,2]], id[,1], fun))
			if (type == 'inner') {
				vv[is.na(ngbdata[2,])] = 0
			} else if (type == 'outer') {
				vv[! is.na(ngbdata[2,])] = 0
			}
			if (asNA) {		
				if (asZero) {
					vv[vv==0 & is.na(ngbdata[2,])] = NA 	
				} else {
					vv[vv==0] = NA 					
				}
			}
			
		} else {
			ngbdata[1:(ngb[1]-1), ] <- ngbdata[2:(ngb[1]), ]
			ngbdata[nrows,] = NA
			nrows=nrows-1
			ids = matrix(as.vector(idx), nrow=ngb[1])
			ids = ids[1:nrows, ]
			ids = matrix(as.vector(ids), ncol=ncol(out))
			ids = cbind(rep(1:ncol(out), each=nrow(ids)), as.vector(ids))
			ids = subset(ids, ids[,2]>0)
			vv = as.vector(tapply(as.vector(ngbdata)[ids[,2]], ids[,1], fun))
			if (type == 'inner') {
				vv[is.na(ngbdata[1,])] = 0
			} else if (type == 'outer') {
				vv[! is.na(ngbdata[1,])] = 0		
			} 
			if (asNA) {		
				if (asZero) {
					vv[vv==0 & is.na(ngbdata[1,])] = NA 	
				} else {
					vv[vv==0] = NA 					
				}
			}			
		}
		
		if (inmem) {
			v[,r] <- vv
		} else {
			out <- writeValues(out, vv, r)
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
)	

