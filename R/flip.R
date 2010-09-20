# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("flip")) {
	setGeneric("flip", function(x, direction, ...)
		standardGeneric("flip"))
}


setMethod('flip', signature(x='RasterLayer', direction='ANY'), 
	function(x, direction='y', filename='', ...)  {
	
		filename <- trim(filename)
		outRaster <- raster(x)

		if (direction[1] == 1) { direction <- 'x'
		} else if (direction[1] == 2) { direction <- 'y' }
		if (!(direction %in% c('y', 'x'))) {
			stop('directions should be y or x')
		}
	
		if (!canProcessInMemory(outRaster, 2) && filename == '') {
			filename <- rasterTmpFile()
			inmemory = FALSE
		} else {
			inmemory = TRUE
		}
		
		if ( inmemory ) {
			v <- getValues(x, format='matrix')

			if (direction == 'y') {
				v <- v[nrow(v):1,]
			} else {
				v <- v[,ncol(v):1]
			}
			outRaster <- setValues(outRaster, as.vector(t(v)))
			if (filename != '') {
				outRaster = writeRaster(outRaster, filename=filename, ...)
			}
		} else {
			tr <- blockSize(outRaster)
			pb <- pbCreate(tr$n, type=.progress(...))
			outRaster <- writeStart(outRaster, filename=filename, datatype=dataType(x), ... )
			if (direction == 'y') {
				trinv <- tr
				trinv$row <- rev(trinv$row)
				trinv$size <- rev(trinv$size)
				for (i in 1:tr$n) {
					v = getValues(x, row=trinv$row[i], nrows=trinv$size)
					v = matrix(v, ncol=ncol(x), byrow=TRUE)
					v = as.vector(t(v[nrow(v):1, ]))
					outRaster <- writeValues(outRaster, v, tr$row[i])
					pbStep(pb, i) 
				}
			} else {
				for (i in 1:tr$n) {
					v = getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					v = matrix(v, ncol=ncol(x), byrow=TRUE)
					v = as.vector(t(v[, ncol(v):1]))
					outRaster <- writeValues(outRaster, v, tr$row[i])
					pbStep(pb, i) 
				}
			}
			outRaster <- writeStop(outRaster)
			pbClose(pb)
		}
		return(outRaster)
	}
)


