# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
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
			v <- vector(length=ncol(x))
		}

		ry <- yres(x)
		rx <- xres(x)		
		dy <- pointDistance(c(0,0),c(0,ry),'GreatCircle')

		pb <- pbCreate(nrow(x), type=.progress(...))
		for (r in 1:nrow(x)) {
			y <- yFromRow(x, r)
			dx <- pointDistance(c(0,y), c(rx,y), 'GreatCircle')
			if (filename == "") {
				v[,r] <- dx * dy / 1000000
			} else {
				v[] <- dx * dy / 1000000
				x <- setValues(x, v, r)
				x <- writeRaster(x)
			}
			pbStep(pb, r)
		}
		pbClose(pb)
		if (filename == "") { 
			x <- setValues(x, as.vector(v))
		}
		return(x)		
	}
)


