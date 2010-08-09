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
			x <- cbind( x, 1:length(x) )
			if (! subNA) {
				y <- merge(x, y, by=1)
				x[y[,2], 1] <- y[,3]
				return(x[,1])
			} else {
				x <- merge(x, y, by=1, all.x=TRUE)
				x <- x[order(x[,2]), 3]
				return(x)
			}
		}

		y <- y[ , c(by, which)]

		tt <- table(y[,1])
		tt <- tt[ which(tt > 1) ]
		if (length(tt) > 0) {
			stop('duplicate "by" values not allowed')
		}

		r <- raster(x)
		
		filename <- trim(filename)
		
		if (canProcessInMemory(x, 3)) {
			r <- setValues(r, localmerge( getValues(x), y, subsWithNA))
			if (filename != '') {
				r <- writeRaster(r, filename=filename, ...)
			}
			return(r)
			
		} else {
			if (filename == '') {
				filename <- rasterTmpFile()
			}
			tr <- blockSize(r)
			pb <- pbCreate(tr$n, type=.progress(...))
			r <- writeStart(r, filename=filename, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$size)
				r <- writeValues(r, localmerge(v, y, subsWithNA), tr$row[i])
				pbStep(pb) 
			}
			pbClose(pb)			
			r <- writeStop(r)
			return(r)
		}
	}
)


