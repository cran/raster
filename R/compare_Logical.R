# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


.asLogical <- function(x) {
	x[x!=0] <- 1
	return(x)
}



setMethod('==', signature(e1='BasicRaster', e2='BasicRaster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), extent=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.05, stopiffalse=FALSE) 
		return(cond)
	}
)	


setMethod('!=', signature(e1='BasicRaster', e2='BasicRaster'),
	function(e1,e2){
		cond <- compare(c(e1, e2), extent=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.05, stopiffalse=FALSE) 
		return(!cond)
	}
)	




setMethod('!', signature(x='RasterLayer'),
	function(x){
		r <- raster(x)
		if (canProcessInMemory(r, 3)) {
			dataType(x) <- 'LOG1S'
			return(setValues(r, ! getValues(x)))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- ! .asLogical(getValues(x, row=tr$row[i], nrows=tr$size))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)		
		}
	}
)	



setMethod("Compare", signature(e1='RasterLayer', e2='logical'),
	function(e1,e2){
		r <- raster(e1)
		dataType(r) <- 'LOG1S'
		if (canProcessInMemory(r, 3)) {
			r <- setValues(r, values=callGeneric(getValues(e1), e2 ) )			
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e1, row=tr$row[i], nrows=tr$size), e2)
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)



setMethod("Compare", signature(e1='logical', e2='RasterLayer'),
	function(e1,e2){
		r <- raster(e2)
		dataType(r) <- 'LOG1S'
		if (canProcessInMemory(r, 3)) {
			r <- setValues(r, values=callGeneric(getValues(e2), e1 ) )			
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e2, row=tr$row[i], nrows=tr$size), e1)
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)




setMethod("Compare", signature(e1='RasterLayer', e2='numeric'),
	function(e1,e2){
		if (!isTRUE(is.atomic(e2) & length(e2)==1)) {
			stop('second argument should be a single number')
		}
		r <- raster(e1)
		dataType(r) <- 'LOG1S'
		if (canProcessInMemory(r, 3)) {
			r <- setValues(r, values=callGeneric(getValues(e1), e2))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e1, row=tr$row[i], nrows=tr$size), e2)
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)	


setMethod("Compare", signature(e1='numeric', e2='RasterLayer'),
	function(e1,e2){
		r <- raster(e2)
		dataType(r) <- 'LOG1S'
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, callGeneric(getValues(e2), rep(e1, ncell(e2)) ) )
		} else {
		
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e2, row=tr$row[i], nrows=tr$size), e1)
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
	
		}
		return(r)
	}
)	

setMethod("Compare", signature(e1='RasterLayer', e2='RasterLayer'),
	function(e1,e2){
		
		compare(c(e1, e2), extent=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=TRUE) 
		
		r <- raster(e1) 
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, callGeneric(getValues(e1), getValues(e2) ) ) 
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e1, row=tr$row[i], nrows=tr$size), getValues(e2, row=tr$row[i], nrows=tr$size))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)	



setMethod("Logic", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		r <- raster(e1)
		cond <- compare(c(r, e2), extent=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		if (!cond) {
			stop("Cannot compare RasterLayers that have different BasicRaster attributes. See compare()")
		}	
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, callGeneric(.asLogical(getValues(e1)), .asLogical(getValues(e2))))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(.asLogical(getValues(e1, row=tr$row[i], nrows=tr$size)), .asLogical(getValues(e2, row=tr$row[i], nrows=tr$size)))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}	
		return(r)
	}
)


setMethod("Compare", signature(e1='Extent', e2='Extent'),
	function(e1,e2){
		a <- callGeneric(e2@xmin, e1@xmin)
		b <- callGeneric(e1@xmax, e2@xmax)
		c <- callGeneric(e2@ymin, e1@ymin)
		d <- callGeneric(e1@ymax, e2@ymax)
		a & b & c & d
	}
)	

