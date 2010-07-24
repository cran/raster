# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

 
 if (!isGeneric("dropLayer")) {
	setGeneric("dropLayer", function(x, i)
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
function(x, i) {
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
function(x, i) {
	if (is.character(i)) {
		i = .nameToIndex(i, layerNames(x))
	}
	i <- sort(unique(round(i)))
	i <- i[i > 0]
	i <- i[i < (nlayers(x)+1)]
	if (length(i) > 0) {

		if ( fromDisk(x) ) {
			if (! inMemory(x) ) {
				x = try(readAll(x))
				if (class(x) == 'try-error') {
					stop('cannot yet drop a layer from a brick that cannot be loaded into memory')
				}
			}
		}
		x@layernames <- x@layernames[-i]
		x@data@values <- x@data@values[,-i]
	}
	return(x)
}
)
