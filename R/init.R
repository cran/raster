# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

init <- function(raster, fun, v, filename="", ...) {
	if (missing(fun) & missing(v)) {
		stop('provide either a function "fun" or an option "id"')
	}
	if (missing(fun)) {
		v = tolower(v[1])
		if (! v %in% c('x', 'y', 'row', 'col', 'cell')) {
			stop('provide either a function "fun" or an option "id"')
		}
	}

	outRaster <- raster(raster)
	filename <- trim(filename)
	
	inmem=TRUE
	if (!canProcessInMemory(outRaster, 2)) {
		inmem=FALSE
		if (filename == '') {
			filename <- rasterTmpFile()
									
		}
	}
	
	if (missing(fun)) {
		if ( inmem ) {
			if (v == 'cell') { outRaster <- setValues(outRaster, 1:ncell(outRaster)) 
			} else if (v == 'row') { outRaster <- setValues(outRaster, rep(1:nrow(outRaster), each=ncol(outRaster)))
			} else if (v == 'y') { outRaster <- setValues(outRaster, rep(yFromRow(outRaster, 1:nrow(outRaster)), each=ncol(outRaster)))
			} else if (v == 'col') { outRaster <- setValues(outRaster, rep(1:ncol(outRaster), times=nrow(outRaster)))
			} else if (v == 'x') { outRaster <- setValues(outRaster, rep(xFromCol(outRaster, 1:ncol(outRaster)), times=nrow(outRaster))) }
		} else {
			outRaster <- writeStart(outRaster, filename=filename, ...)
			pb <- pbCreate(nrow(outRaster), type=.progress(...))
			for (r in 1:nrow(outRaster)) {
				if (v == 'cell') { outRaster <- writeValues(outRaster, cellFromRowCol(r,1):cellFromRowCol(r,ncol(outRaster)), r)
				} else if (v == 'row') {outRaster <-  writeValues(outRaster, rep(r, each=ncol(outRaster)), r)
				} else if (v == 'y') { outRaster <- writeValues(outRaster, rep(yFromRow(outRaster, r), each=ncol(outRaster)), r)
				} else if (v == 'col') { outRaster <- writeValues(outRaster, 1:ncol(outRaster), r)
				} else if (v == 'x') { outRaster <- writeValues(outRaster, xFromCol(1:ncol(outRaster)), r) }
				pbStep(pb, r)
			}
			pbClose(pb)
			outRaster <- writeStop(outRaster)
		}
	} else {
		if ( inmem ) {
			n <- ncell(raster)
			outRaster <- setValues(outRaster, fun(n)) 
		} else {
			n <- ncol(raster)
			outRaster <- writeStart(outRaster, filename=filename, ...)
			pb <- pbCreate(nrow(outRaster), type=.progress(...))
			for (r in 1:nrow(raster)) {
				outRaster <- writeValues(outRaster, fun(n), r) 
				pbStep(pb, r)
			}
			pbClose(pb)
			outRaster <- writeStop(outRaster)
		}
	}
	if (inmem & filename != '') {
		outRaster = writeRaster(outRaster, filename=filename, ...)
	}
	return(outRaster)
}

