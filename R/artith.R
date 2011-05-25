# Author: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3

setMethod("Arith", signature(e1='Raster', e2='Raster'),
    function(e1, e2){ 

		if (!hasValues(e1)) { stop('first Raster object has no values') }
		if (!hasValues(e2)) { stop('second Raster object has no values') }
		
		nl1 <- nlayers(e1)
		nl2 <- nlayers(e2)
		nl <- max(nl1, nl2)

		
		if (nl > 1) {
			r <- brick(e1, values=FALSE)
		} else {
			r <- raster(e1)
		}
		
		if ( ! compare(e1, e2, stopiffalse=FALSE) ) {
			if ( compare(e1, e2, extent=FALSE, rowcol=FALSE, prj=TRUE, res=TRUE, orig=TRUE, stopiffalse=TRUE) ) {
				ie <- intersectExtent(extent(e1), extent(e2), validate=FALSE)
				if (is.null(ie)) {
					stop('Layers have non-overlapping extents')
				}
				warning('Raster objects have different extents. Result for their intersection is returned')
				e1 <- crop(e1, ie)
				e2 <- crop(e2, ie)
			} else {
				stop()  # stops anyway because compare returned FALSE
			}
		}
		
		if (canProcessInMemory(r, 4)) {
			if (nl1 == nl2 ) {
				return( setValues(r, values=callGeneric( getValues(e1), getValues(e2))) )
			} else {
				return( setValues(r, matrix(callGeneric( as.vector(getValues(e1)), as.vector(getValues(e2))), ncol=nl)) )
			}
			
		} else {
		
			tr <- blockSize(e1)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
			if (nl1 == nl2 ) {
				for (i in 1:tr$n) {
					v1 <- getValues(e1, row=tr$row[i], nrows=tr$nrows[i])
					v2 <- getValues(e2, row=tr$row[i], nrows=tr$nrows[i])
					v <- callGeneric( v1, v2 )
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 	
				}
			} else {
				for (i in 1:tr$n) {
					v1 <- as.vector(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]))
					v2 <- as.vector(getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))
					v <- matrix(callGeneric( v1, v2 ), ncol=nl)
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 	
				}
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)
			
		}
	}	
)




setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		if (!hasValues(e1)) { stop('RasterLayer has no values') }

		r <- raster(e1)
		if (canProcessInMemory(e1, 4)) {
			return ( setValues(r,  callGeneric(as.numeric(getValues(e1)), e2) ) )
		} else {
			tr <- blockSize(e1)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- getValues(e1, row=tr$row[i], nrows=tr$nrows[i])
				v <- callGeneric( v, e2 )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 	
			}
			r <- writeStop(r)
			pbClose(pb)

			return(r)
		}		
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
# simpler code, but would this make another copy of the objects?
#		callGeneric(e2, e1) 
		if (!hasValues(e2)) { stop('RasterLayer has no values') }

		r <- raster(e2)
		if (canProcessInMemory(e2, 4)) {
			return( setValues(r, callGeneric(as.numeric(e1), getValues(e2))) )
		} else {
			tr <- blockSize(e2)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- getValues(e2, row=tr$row[i], nrows=tr$nrows[i])
				v <- callGeneric( v, e1 )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 	
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)
		}		
	}
)


setMethod("Arith", signature(e1='RasterLayer', e2='logical'),
    function(e1, e2){ 
		e2 <- as.integer(e2)
		callGeneric(e1, e2)
	}
)

setMethod("Arith", signature(e1='logical', e2='RasterLayer'),
    function(e1, e2){ 
		e1 <- as.integer(e1)
		callGeneric(e1, e2)
	}
)



setMethod("Arith", signature(e1='RasterStackBrick', e2='numeric'),
    function(e1, e2) {
	
		if (length(e2) > 1) {
			if (length(e2) != nlayers(e1)) {
				stop('length of e2 > 1 but not equal to nlayers(e1)')
			}
					
			if (canProcessInMemory(e1, 3)) {
				b <- brick(e1, values=FALSE)
				return( setValues(b, t(callGeneric( t(getValues(e1)), e2))) )
			}
			
			filename <- rasterTmpFile()
			b <- brick(e1, values=FALSE)
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, type=.progress())
			b <- writeStart(b, filename=filename, bandorder='BIL')
			for (i in 1:tr$n) {
				v <- t (callGeneric( t(getValues(e1, row=tr$row[i], nrows=tr$nrows[i])), e2) )
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			pbClose(pb)
			return(b)
		}
		
		# else:
		
		if (canProcessInMemory(e1, 3)) {
			b <- brick(e1, values=FALSE)
			return ( setValues(b,  callGeneric(getValues(e1), e2) ) )
		} else {
			filename <- rasterTmpFile()
			b <- brick(e1, values=FALSE)
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, type=.progress())
			b <- writeStart(b, filename=filename, bandorder='BIL')
			for (i in 1:tr$n) {
				v <- callGeneric( getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e2)
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			pbClose(pb)
			return(b)
		}
	}
)



setMethod("Arith", signature(e1='numeric', e2='RasterStackBrick'),
    function(e1, e2){ 
		callGeneric(e2, e1) 
	}
)


setMethod("Arith", signature(e1='RasterStackBrick', e2='logical'),
    function(e1, e2){ 
		e2 <- as.integer(e2)
		callGeneric(e1, e2)
	}
)

setMethod("Arith", signature(e1='logical', e2='RasterStackBrick'),
    function(e1, e2){ 
		e1 <- as.integer(e1)
		callGeneric(e1, e2)
	}
)


setMethod("Arith", signature(e1='Extent', e2='numeric'),
	function(e1, e2){ 
	
		if (length(e2) == 1) {
			x1 = e2
			x2 = e2
		} else if (length(e2) == 2) {
			x1 = e2[1]
			x2 = e2[2]
		} else {
			stop('On an Extent object, you can only use Arith with a single number or with two numbers')
		}

		r <- e1@xmax - e1@xmin
		d <- callGeneric(r, x1)
		d <- (d - r) / 2
		e1@xmax <- e1@xmax + d
		e1@xmin <- e1@xmin - d
		
		r <- e1@ymax - e1@ymin
		d <- callGeneric(r, x2)
		d <- (d - r) / 2
		e1@ymax <- e1@ymax + d
		e1@ymin <- e1@ymin - d
		
		return(e1)
	}
)

setMethod("Arith", signature(e1='numeric', e2='Extent'),
    function(e1, e2){ 
		callGeneric(e2,e1)
	}
)

