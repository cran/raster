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

	outraster <- raster(raster)
	filename <- trim(filename)
	
	inmem=TRUE
	if (!canProcessInMemory(outraster, 2)) {
		inmem=FALSE
		if (filename == '') {
			filename <- rasterTmpFile()
									
		}
	}
	
	if (missing(fun)) {
		if ( inmem ) {
			if (v == 'cell') { outraster <- setValues(outraster, 1:ncell(outraster)) 
			} else if (v == 'row') { outraster <- setValues(outraster, rep(1:nrow(outraster), each=ncol(outraster)))
			} else if (v == 'y') { outraster <- setValues(outraster, rep(yFromRow(outraster, 1:nrow(outraster)), each=ncol(outraster)))
			} else if (v == 'col') { outraster <- setValues(outraster, rep(1:ncol(outraster), times=nrow(outraster)))
			} else if (v == 'x') { outraster <- setValues(outraster, rep(xFromCol(outraster, 1:ncol(outraster)), times=nrow(outraster))) }
		} else {
			for (r in 1:nrow(raster)) {
				if (v == 'cell') { outraster <- setValues(outraster, cellFromRowCol(r,1):cellFromRowCol(r,ncol(outraster)), r)
				} else if (v == 'row') { outraster <- setValues(outraster, rep(r, each=ncol(outraster)), r)
				} else if (v == 'y') { outraster <- setValues(outraster, rep(yFromRow(outraster, r), each=ncol(outraster)), r)
				} else if (v == 'col') { outraster <- setValues(outraster, 1:ncol(outraster), r)
				} else if (v == 'x') { outraster <- setValues(outraster, xFromCol(1:ncol(outraster)), r) }
				outraster <- writeRaster(outraster, filename=filename, doPB=TRUE, ...)
			}	
		}
	} else {
		if ( inmem ) {
			n <- ncell(raster)
			outraster <- setValues(outraster, fun(n)) 
		} else {
			n <- ncol(raster)
			for (r in 1:nrow(raster)) {
				outraster <- setValues(outraster, fun(n), r) 
				outraster <- writeRaster(outraster, filename=filename, doPB=TRUE, ...)
			}
		}
	}
	if (inmem & filename != '') {
		outraster = writeRaster(outraster, filename=filename, ...)
	}
	return(outraster)
}

