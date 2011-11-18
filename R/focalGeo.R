# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : April 2011
# Version 1.0
# Licence GPL v3

.focalGeo <- function(x, d, fun=sum, filename="", na.rm=FALSE, pad=TRUE, padValue=NA, ...) {

	stopifnot(nlayers(x) == 1)

	out <- raster(x)

	if (! .couldBeLonLat(out)) {
		stop("not a lon/lat raster")
	}
	
	bufy <- max(1, min(nrow(out), round((d / 111319.5) / yres(out))))
	lat <- yFromRow(out, 1:nrow(out))
	bufx <- pmax(1, pmin(ncol(out), round( d / pointDistance(cbind(0, lat), cbind(1, lat), longlat=TRUE) / xres(out))))
	
	
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

	rows <- list()
	for (r in 2:(bufy)) {
		ngb <- c(1, bufx[r])
		rows[[r]] <- .focalValues(x, row=r, ngb=ngb, fun=NULL) 
	}	

	for (r in bufy:nrow(out)) {
		rows[1:(bufy-1)] <- rows[2:bufy]
		rows[[bufy]] <- .focalValues(x, row=r, ngb=ngb, fun=NULL) 
		m <- do.call("rbind", rows)
		m <- as.vector( tapply(m[,2], m[,1], FUN=fun, na.rm=na.rm) 	)
		if (inMem) {
			v[,r] <- m
		} else {
			out <- writeValues(out, m, r)
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

