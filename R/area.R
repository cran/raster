# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : December 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("area")) {
	setGeneric("area", function(x, ...)
		standardGeneric("area"))
}	


setMethod('area', signature(x='Raster'), 
	function(x, filename='', ...) {
		x = raster(x)
	
		if (! .couldBeLonLat(x)) {
			stop('This function is only useful for layer with a longitude/latitude coordinates')
		}
	
		filename <- trim(filename)
		if (!canProcessInMemory(x, 3) & filename == '') {
			filename <- rasterTmpFile()
		}
		
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(x), nrow=ncol(x))
		} else {
			x <- writeStart(x, filename=filename, ...)
		}

		dy <- pointDistance(c(0,0),c(0, yres(x) ),'GreatCircle')
		y <- yFromRow(x, 1:nrow(x))
		dx <- pointDistance(cbind(0, y), cbind(xres(x), y), 'GreatCircle')

		tr <- blockSize(x)
		pb <- pbCreate(tr$n, type=.progress(...))

		for (i in 1:tr$n) {
			r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
			vv <- dx[r] * dy / 1000000
			vv <- rep(vv, each=ncol(x))
			if (filename == "") {
				v[,r] <- vv
			} else {
				x <- writeValues(x, vv, tr$row[i])
			}
			pbStep(pb, i)
		}
		pbClose(pb)
		
		if (filename == "") { 
			x <- setValues(x, as.vector(v))
		} else {
			x <- writeStop(x)	
		}
		return(x)		
	}
)


