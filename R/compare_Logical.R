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




setMethod('!', signature(x='Raster'),
	function(x){
		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			return(setValues(r, ! getValues(x)))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- ! .asLogical(getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)		
		}
	}
)	



setMethod("Compare", signature(e1='Raster', e2='logical'),
	function(e1,e2){
		if (nlayers(e1) > 1) {
			r <- brick(e1, values=FALSE)
		} else {
			r <- raster(e1)
		}

		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, values=callGeneric(getValues(e1), e2 ) )			
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e2)
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)



setMethod("Compare", signature(e1='logical', e2='Raster'),
	function(e1,e2){
		if (nlayers(e2) > 1) {
			r <- brick(e2, values=FALSE)
		} else {
			r <- raster(e2)
		}

		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, values=callGeneric(getValues(e2), e1 ) )			
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e2, row=tr$row[i], nrows=tr$nrows[i]), e1)
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)




setMethod("Compare", signature(e1='Raster', e2='numeric'),
	function(e1,e2){

		if (nlayers(e1) > 1) {
			r <- brick(e1, values=FALSE)
		} else {
			r <- raster(e1)
		}

		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, values=callGeneric(getValues(e1), e2))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e2)
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)	


setMethod("Compare", signature(e1='numeric', e2='Raster'),
	function(e1,e2){
	
		if (nlayers(e2) > 1) {
			r <- brick(e2, values=FALSE)
		} else {
			r <- raster(e2)
		}

		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, callGeneric(getValues(e2), rep(e1, ncell(e2)) ) )
		} else {
		
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e2, row=tr$row[i], nrows=tr$nrows[i]), e1)
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
	
		}
		return(r)
	}
)	

setMethod("Compare", signature(e1='Raster', e2='Raster'),
	function(e1,e2){
		
		compare(c(e1, e2), extent=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=TRUE) 
		
		if (nlayers(e1) > 1) {
			if (nlayers(e2) > 1 & nlayers(e2) != nlayers(e1)) {
				stop('number of layers of objects do not match')
			}
			r <- brick(e1, values=FALSE)
		} else if (nlayers(e2) > 1) {
			r <- brick(e2, values=FALSE)
		} else {
			r <- raster(e1)
		}
		
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, callGeneric(getValues(e1), getValues(e2) ) ) 
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)	



setMethod("Logic", signature(e1='Raster', e2='Raster'),
    function(e1, e2){ 
	
		if (nlayers(e1) > 1) {
			r <- brick(e1, values=FALSE)
			if (nlayers(e2) > 1 & nlayers(e2) != nlayers(e1)) {
				stop('number of layers of objects do not match')
			}
		} else if (nlayers(e2) > 1) {
			r <- brick(e2, values=FALSE)
		} else {
			r <- raster(e1)
		}
	
		cond <- compare(c(r, e2), extent=TRUE, rowcol=TRUE, prj=TRUE, tolerance=0.0001, stopiffalse=FALSE) 
		if (!cond) {
			stop("Cannot compare Rasters that have different BasicRaster attributes. See compare()")
		}	
		
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, callGeneric(.asLogical(getValues(e1)), .asLogical(getValues(e2))))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress())
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- callGeneric(.asLogical(getValues(e1, row=tr$row[i], nrows=tr$nrows[i])), .asLogical(getValues(e2, row=tr$row[i], nrows=tr$nrows[i])))
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

