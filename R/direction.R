# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("direction")) {
	setGeneric("direction", function(x, ...)
		standardGeneric("direction"))
}	


setMethod('direction', signature(x='RasterLayer'), 

function(x, fun=NULL, from=FALSE, filename='', ...) {

	out <- raster(x)
	
	if (.couldBeLonLat(out)) { 
		longlat=TRUE
		if (is.null(fun)) { 
			direct <- function (p1, p2) {
				toRad <- pi / 180 
				p1 <- p1 * toRad
				p2 <- p2 * toRad
				dLon = p2[1] - p1[1] 
				y = sin(dLon)  * cos(p2[2]) 
				x = cos(p1[2]) * sin(p2[2]) - sin(p1[2]) * cos(p2[2]) * cos(dLon) 
				azm = atan2(y, x) / toRad
				azm[azm < 0] <-  360 + azm[azm < 0] 
				return(azm)
			}
		} else {
			direct <- function (p1, p2) {
				toRad <- pi / 180 
				p1 <- p1 * toRad
				p2 <- p2 * toRad
				dLon = p2[1] - p1[1] 
				y = sin(dLon)  * cos(p2[2]) 
				x = cos(p1[2]) * sin(p2[2]) - sin(p1[2]) * cos(p2[2]) * cos(dLon) 
				azm = atan2(y, x) 
				azm[azm < 0] <-  azm[azm < 0] + 2 * pi
				return(fun(azm))
			}
		}
	} else { 
		longlat=FALSE
		if (is.null(fun)) { 
			direct <- function(from, to) {
				A = to[2] - from[2]
				B = to[1] - from[1]
				b = atan2(B, A) * 180 / pi 
				return( (b + 360) %% 360 )
			}
		} else {
			direct <- function(from, to) {
				A = to[2] - from[2]
				B = to[1] - from[1]
				b = atan2(B, A)
				return(fun(b))
			}
		}
	}	


	r = edge(x, classes=FALSE, type='inner', asNA=TRUE, progress=.progress(...)) 
	
	pts <- try(  rasterToPoints(r, fun=function(z){ z>0 })[,1:2, drop=FALSE] )
	
	if (class(pts) == "try-error") {
		stop('This function has not yet been implemented for very large files')
	}

	if (nrow(pts) == 0) {
		stop('RasterLayer has no NA cells (for which to compute a direction)')
	}

                                                                        
	filename <- trim(filename)
	if (!canProcessInMemory(out, 2) && filename == '') {
		filename <- rasterTmpFile()					
	}
	
	if (filename == '') {
		v <- matrix(ncol=nrow(out), nrow=ncol(out))
	} else {
		out <- writeStart(out, filename, ...)
	}
	
	xy <- xFromCol(out, 1:ncol(out))
	xy <- cbind(xy, NA)
	
	pb <- pbCreate(nrow(out), type=.progress(...))
	
	for (r in 1:nrow(out)) {	
		vals <- getValues(x, r)
		i = which(is.na(vals))
		vals[] <- NA
		if (length(i) > 0) {
			xy[,2] <- yFromRow(out, r)
			for (c in i) {
				pm <- which.min( pointDistance(xy[c,], pts, longlat=longlat) )
				if (from) {
					vals[c] <- direct(pts[pm, ], xy[c,])
				} else {
					vals[c] <- direct(xy[c,], pts[pm, ])
				}
			}
		}
		if (filename == "") {
			v[,r] <- vals
		} else {
			out <- writeValues(out, vals, r)
		}
		pbStep(pb, r) 	
	}	
	pbClose(pb)
	
	if (filename == "") { 
		out <- setValues(out, as.vector(v)) 
	} else {
		out <- writeStop(out)
	}
	return(out)
}

)


