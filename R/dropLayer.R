# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

 
 if (!isGeneric("dropLayer")) {
	setGeneric("dropLayer", function(x, i, ...)
		standardGeneric("dropLayer"))
}
 

.nameToIndex <- function(name, allnames) {
	k = NULL
	for (i in 1:length(name)) {
		k = c(k, which(allnames == name[i])[1])
	}
	return(k)
}
 
 
setMethod('dropLayer', signature(x='RasterStack'), 
function(x, i, ...) {
	if (is.character(i)) {
		i = .nameToIndex(i, layerNames(x))
	}
	i <- sort(unique(round(i)))
	i <- i[i > 0]
	i <- i[i < (nlayers(x)+1)]
	if (length(i) > 0) {
		x@layers <- x@layers[-i]
		x@layernames <- x@layernames[-i]
	}
	return(x)
}
)


setMethod('dropLayer', signature(x='RasterBrick'), 
function(x, i, filename='', ...) {
	if (is.character(i)) {
		i = .nameToIndex(i, layerNames(x))
	}
	i <- sort(unique(round(i)))
	i <- i[i > 0]
	i <- i[i < (nlayers(x)+1)]

	filename <- trim(filename)

	if (length(i) > 0) {
		if ( fromDisk(x) ) {
			if (! inMemory(x) ) {
				try(x <- readAll(x))
			}
		}
		if (inMemory(x)) {
			x@data@values <- x@data@values[,-i,drop=FALSE]
			x@data@nlayers <- x@data@nlayers - length(i)
			x@data@min <- x@data@min[-i]
			x@data@max <- x@data@max[-i]
			x@data@fromdisk <- FALSE
			x@file@name = ''
			x@layernames <- x@layernames[-i]
			
			if (filename != '') {
				x <- writeRaster(x, filename=filename, ...)
			}
			return(x)
			
		} else {
			if ( filename == '') {	filename <- rasterTmpFile()	} 
			out <- brick(x, values=FALSE)
			out@layernames <- x@layernames[-i]
			out@data@nlayers <- out@data@nlayers - length(i)
			out <- writeStart(out, filename=filename, ...)
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, type=.progress(...))			
	
			for (j in 1:tr$n) {
				vv <- getValues(x, row=tr$row[j], nrows=tr$nrows[j])[, -i] 
				out <- writeValues(out, vv, tr$row[j])
				pbStep(pb, j)
			}
			pbClose(pb)
			out <- writeStop(out)
			return(out)
		}
	}
}
)



