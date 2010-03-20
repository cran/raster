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
			pb <- pbCreate(nrow(outRaster), type=.progress(...))
			if (direction == 'y') {
				readRows = nrow(x):1
				for (r in 1:nrow(outRaster)) {
					res <- getValues(x, readRows[r])
					outRaster <- setValues(outRaster, res, r)
					outRaster <- writeRaster(outRaster, filename=filename, ...)
					pbStep(pb, r)
				}
			} else {
				for (r in 1:nrow(outRaster)) {
					res <- getValues(x, r)
					outRaster <- setValues(outRaster, rev(res), r)
					outRaster <- writeRaster(outRaster, filename=filename, ...)
					pbStep(pb, r)
				}
			}
			pbClose(pb)
		}
		return(outRaster)
	}
)


