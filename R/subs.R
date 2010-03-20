# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  February 2010
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("subs")) {
	setGeneric("subs", function(x, y, ...)
		standardGeneric("subs"))
}

setMethod('subs', signature(x='RasterLayer', y='data.frame'), 
	function(x, y, by=1, which=2, subsWithNA=TRUE, filename='', ...) { 
	
		localmerge <- function(x, y, subNA) {
			x = cbind(x, 1:length(x))
			if (! subNA) {
				y = merge(x, y, by=1, all.x=TRUE)
				y = y[order(y[,2]), 3]	
				y[is.na(y)] = x[is.na(y),1]
				return(y)
			} else {
				x = merge(x, y, by=1, all.x=TRUE)
				x = x[order(x[,2]), 3]
				return(x)
			}
		}

		y = y[ , c(by, which)]
		r = raster(x)
		
		if (canProcessInMemory(x, 3)) {
			return ( setValues(r, localmerge( getValues(x), y, subsWithNA)) )
			
		} else {
			if (trim(filename) == '') filename <- rasterTmpFile()
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress(...))
			r <- writeStart(r, filename=filename, ... )
			for (i in 1:tr$n) {
				v = getValuesBlock(x, row=tr$row[i], nrows=tr$size)
				writeValues(r, localmerge(v, y, subsWithNA), tr$row[i])
				pbStep(pb, i) 
			}
			pbClose(pb)			
			return( writeStop(r) )
		}
	}
)


