# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("Math", signature(x='Raster'),
    function(x){ 
		fname <- as.character(sys.call(sys.parent())[[1]])

		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

		
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


setMethod("Math2", signature(x='Raster'), 
	function (x, digits=0) {
		digits <- max(0, digits)

		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

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


