# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("area")) {
	setGeneric("area", function(x, ...)
		standardGeneric("area"))
}	


setMethod('area', signature(x='RasterLayer'), 
	function(x, filename='', na.rm=FALSE, weights=FALSE, ...) {

		out = raster(x)
	
		if (na.rm) {
			if (! hasValues(x) ) {
				na.rm <- FALSE
				warning("'x' has no values, ignoring 'na.rm=TRUE'")
				rm(x)
			}
		} else {
			rm(x)
		}	
	
		if (! .couldBeLonLat(out)) {
			stop('This function is only useful for layer with a longitude/latitude coordinates')
		}
	
		filename <- trim(filename)
		if (!canProcessInMemory(out, 3) & filename == '') {
			filename <- rasterTmpFile()
		}
		
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
		} else {
			if (weights) {
				outfname = filename
				filename = rasterTmpFile()
			}
			out <- writeStart(out, filename=filename, ...)
		}

		dy <- pointDistance(c(0,0),c(0, yres(out) ),'GreatCircle')
		y <- yFromRow(out, 1:nrow(out))
		dx <- pointDistance(cbind(0, y), cbind(xres(out), y), 'GreatCircle')

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, type=.progress(...))

		for (i in 1:tr$n) {
			r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
			vv <- dx[r] * dy / 1000000
			vv <- rep(vv, each=out@ncols)
			if (na.rm) {
				a <- getValues(x, tr$row[i], tr$nrows[i])
				vv[is.na(a)] <- NA
			}
			if (filename == "") {
				v[,r] <- vv
			} else {
				out <- writeValues(out, vv, tr$row[i])
			}
			pbStep(pb, i)
		}
		pbClose(pb)
		
		if (filename == "") { 
			v <- as.vector(v)
			if (weights) {
				v <- v / sum(v, na.rm=TRUE)
			}
			out <- setValues(out, v)
		} else {
			out <- writeStop(out)	
			if (weights) {
				total <- cellStats(out, 'sum')
				out <- calc(out, fun=function(x){x/total}, filename=outfname, ...)
			}
		}
		
		return(out)
	}
)



setMethod('area', signature(x='RasterStackBrick'), 
	function(x, filename='', na.rm=FALSE, weights=FALSE, ...) {

		if (! na.rm) {
			return( area(raster(x), filename=filename, na.rm=FALSE, weights=weights, ...) )
		}	
		
		out = brick(x, values=FALSE)

		if (! .couldBeLonLat(out)) {
			stop('This function is only useful for layer with a longitude/latitude coordinates')
		}
	
		filename <- trim(filename)
		if (!canProcessInMemory(out) & filename == '') {
			filename <- rasterTmpFile()
		}

		nl <- nlayers(out)
		
		if (filename == '') {
			v <- matrix(NA, ncol=nl, nrow=ncell(out))
		} else {
			if (weights) {
				outfname = filename
				filename = rasterTmpFile()
			}
			out <- writeStart(out, filename=filename, ...)
		}

		dy <- pointDistance(c(0,0),c(0, yres(out) ),'GreatCircle')
		y <- yFromRow(out, 1:nrow(out))
		dx <- pointDistance(cbind(0, y), cbind(xres(out), y), 'GreatCircle')

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, type=.progress(...))
		
		#rows <- 1
		for (i in 1:tr$n) {
			r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
			vv <- dx[r] * dy / 1000000
			vv <- rep(vv, each=out@ncols)
			
			vv <- matrix(rep(vv, times=nl), ncol=nl)
			a <- getValues(x, tr$row[i], tr$nrows[i])
			vv[is.na(a)] <- NA

			if (filename == "") {
				start <- (r[1]-1) * ncol(out) + 1
				end <- r[length(r)] * ncol(out) 
				v[start:end, ] <- vv
			} else {
				out <- writeValues(out, vvv, tr$row[i])
				
				#for (j in 1:tr$nrows[i]) {
				#	jj <- 1:ncol(out) + (j-1) * ncol(out)
				#	vvv <- vv[jj,]
				#	out <- writeValues(out, vvv, rows)
				#	rows <- rows + 1
				#}
			}
			pbStep(pb, i)
		}
		pbClose(pb)
		
		if (filename == "") { 
			if (weights) {
				total <- colSums(v, na.rm=TRUE)
				v <- t( t(v) / total )
			}
			out <- setValues(out, v)
		} else {
			out <- writeStop(out)	
			if (weights) {
				total <- cellStats(out, 'sum')
				out <- calc(out, fun=function(x){x / total}, filename=outfname, ...)
			}
		}
		
		return(out)
	}
)


