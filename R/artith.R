# Author: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


	
setMethod("Arith", signature(e1='RasterLayer', e2='RasterLayer'),
    function(e1, e2){ 
		
		if ( ! compare(e1, e2, stopiffalse=FALSE) ) {
			if ( compare(e1, e2, extent=FALSE, rowcol=FALSE, prj=TRUE, res=TRUE, orig=TRUE, stopiffalse=TRUE) ) {
				ie <- intersectExtent(extent(e1), extent(e2), validate=FALSE)
				if (is.null(ie)) {
					stop('Layers have non-overlapping extents')
				}
				warning('RasterLayer objects have different extents. Result for their intersection is returned')
				e1 <- crop(e1, ie)
				e2 <- crop(e2, ie)
			} else {
				stop()  # stops anyway because compare returned FALSE
			}
		}
		
		r <- raster(e1)
		
		if (canProcessInMemory(r, 4)) {
		
			return( setValues(r, values=callGeneric( getValues(e1), getValues(e2))) )
			
		} else {
		
			tr <- blockSize(e1)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v1 <- getValuesBlock(e1, row=tr$row[i], nrows=tr$size)
				v2 <- getValuesBlock(e2, row=tr$row[i], nrows=tr$size)
				v <- callGeneric( v1, v2 )
				writeValues(r, v, tr$row[i])
				pbStep(pb, i) 	
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)
			
		}
	}	
)


setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		r <- raster(e1)
		if (canProcessInMemory(e1, 4)) {
			return ( setValues(r,  callGeneric(as.numeric(getValues(e1)), e2) ) )
		} else {
			tr <- blockSize(e1)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- getValuesBlock(e1, row=tr$row[i], nrows=tr$size)
				v <- callGeneric( v, e2 )
				writeValues(r, v, tr$row[i])
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

		r <- raster(e2)
		if (canProcessInMemory(e2, 4)) {
			return( setValues(r, callGeneric(as.numeric(e1), getValues(e2))) )
		} else {
			tr <- blockSize(e1)
			pb <- pbCreate(tr$n, type=.progress())			
			r <- writeStart(r, filename=rasterTmpFile(), format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- getValuesBlock(e2, row=tr$row[i], nrows=tr$size)
				v <- callGeneric( v, e1 )
				writeValues(r, v, tr$row[i])
				pbStep(pb, i) 	
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)
		}		
	}
)



setMethod("Arith", signature(e1='RasterBrick', e2='numeric'),
    function(e1, e2){ 
		if (canProcessInMemory(e1, 4)) {
			return ( setValues(e1,  callGeneric(getValues(e1), e2) ) )
		} else {
			r <- brick(e1)
			filename <- rasterTmpFile()
			for (row in 1:nrow(e1)) {
				r <- setValues(r, callGeneric( getValues(e1, row), e2) , row) 
				r <- writeRaster(r, filename=filename, doPB=TRUE)
			}
			return(r)
		}		
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterBrick'),
    function(e1, e2){ 
		callGeneric(e2, e1) 
	}
)




setMethod("Arith", signature(e1='Extent', e2='numeric'),
	function(e1, e2){ 
		r <- e1@xmax - e1@xmin
		d <- callGeneric(r, e2)
		d <- (d - r) / 2
		e1@xmax <- e1@xmax + d
		e1@xmin <- e1@xmin - d
		r <- e1@ymax - e1@ymin
		d <- callGeneric(r, e2)
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

