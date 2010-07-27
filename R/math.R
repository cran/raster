# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("Math", signature(x='Raster'),
    function(x){ 
		stop('This function has not been defined for the class of this object')
	}
)
setMethod("Math2", signature(x='Raster'),
    function(x, digits=0){ 
		stop('This function has not been defined for the class of this object')
	}
)


setMethod("Math", signature(x='RasterLayer'),
    function(x){ 
		fname <- as.character(sys.call(sys.parent())[[1]])
		r <- raster(x)
		if (canProcessInMemory(r, 3)) {
			r <- setValues(r, callGeneric(getValues(x)))
		} else {
			if (fname %in% c('floor', 'ceiling', 'trunc')) {
				datatype <- 'INT4S'
			} else {
				datatype <- .datatype()
			}
			
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), datatype=datatype, format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric( getValuesBlock(x, row=tr$row[i], nrows=tr$size) )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
			
		}
		return(r)
	}
)


setMethod("Math2", signature(x='RasterLayer'), 
	function (x, digits=0) {
		digits <- max(0, digits)
		r <- raster(x)
		if (canProcessInMemory(r, 3)) {
			r <- setValues(r, callGeneric( getValues(x), digits))
		} else {
			if (digits == 0) {
				datatype <- 'INT4S'
			} else {
				datatype <- .datatype()
			}

			tr <- blockSize(x)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), datatype=datatype, format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric( getValuesBlock(x, row=tr$row[i], nrows=tr$size), digits )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)

