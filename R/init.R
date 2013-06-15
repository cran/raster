# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

init <- function(x, fun, v, filename="", ...) {
	if (missing(fun) & missing(v)) {
		stop('provide either a function "fun" or an option "v"')
	}
	if (missing(fun)) {
		v = tolower(v[1])
		stopifnot (v %in% c('x', 'y', 'row', 'col', 'cell'))
	}

	out <- raster(x)
	filename <- trim(filename)
	
	inmem=TRUE
	if (!canProcessInMemory(out, 2)) {
		inmem=FALSE
		if (filename == '') {
			filename <- rasterTmpFile()									
		}
	}
	
	if (missing(fun)) {
		if ( inmem ) {
			if (v == 'cell') { 
				out <- setValues(out, 1:ncell(out)) 
			} else if (v == 'row') { 
				out <- setValues(out, rep(1:nrow(out), each=ncol(out)))
			} else if (v == 'y') { 
				out <- setValues(out, rep(yFromRow(out, 1:nrow(out)), each=ncol(out)))
			} else if (v == 'col') { 
				out <- setValues(out, rep(1:ncol(out), times=nrow(out)))
			} else if (v == 'x') { 
				out <- setValues(out, rep(xFromCol(out, 1:ncol(out)), times=nrow(out))) 
			}
		} else {
			out <- writeStart(out, filename=filename, ...)
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='init', ...)
			for (i in 1:tr$n) {
				if (v == 'cell') { 
					out <- writeValues(out, cellFromRowCol(out, tr$row[i],1):cellFromRowCol(out, tr$row[i]+tr$nrows[i]-1, ncol(out)), tr$row[i])
				} else if (v == 'row') {
					r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
					out <-  writeValues(out, rep(r, each=ncol(out)), tr$row[i])
				} else if (v == 'col') { 
					out <- writeValues(out, rep(1:ncol(out), tr$nrows[i]), tr$row[i])
				} else if (v == 'x') { 
					out <- writeValues(out, rep(xFromCol(out, 1:ncol(out)), tr$nrows[i]), tr$row[i])
				} else if (v == 'y') { 
					r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)	
					out <- writeValues(out, rep(yFromRow(out, r), each=ncol(out)), tr$row[i])
				}
				pbStep(pb, i)
			}
			pbClose(pb)
			out <- writeStop(out)
		}
	} else {
		if ( inmem ) {
			n <- ncell(out)
			out <- setValues(out, fun(n)) 
		} else {
			out <- writeStart(out, filename=filename, ...)
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='init', ...)
			for (i in 1:tr$n) {
				n <- ncol(out) * tr$nrows[i]
				out <- writeValues(out, fun(n), tr$row[i])
				pbStep(pb, r)
			}
			pbClose(pb)
			out <- writeStop(out)
		}
	}
	if (inmem & filename != '') {
		out <- writeRaster(out, filename=filename, ...)
	}
	return(out)
}

