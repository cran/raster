# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  October 2008
# Version 0.9
# Licence GPL v3


.makeUniqueNames <- function(x, prefix='layer') {

	x[is.na(x)] <- prefix
	x[x==""] <- prefix

	tln <- table(x)
	cnt <- which(tln > 1)
	tln <- tln[cnt]
	if (length(tln) > 0) {
		for (i in 1:length(tln)) {
			j <- which(x == names(tln[i]))
			x[j] <- paste(x[j], '_', 1:tln[i], sep='')
		}
	}
	return(x)
}


.enforceGoodLayerNames <- function(x, prefix='layer', returnNames=FALSE) {
	ln <- x@layernames[1:nlayers(x)]
	
	ln <- .makeUniqueNames(ln, prefix)
	
	if (returnNames) {
		return(ln)
	} else {
		x@layernames <- ln
		return(x)
	}
}


layerNames <- function(object) {
	ln <- object@layernames
	ln <- ln[1:nlayers(object)]
	ln[is.na(ln)] <- ""
	return(as.vector(ln))
}


'layerNames<-' <- function(object, value) {
	if (length(value) != nlayers(object)) {
		stop('value has wrong length')
	}
	value <- as.vector(value)
	if (inherits(object, "RasterLayer")) {
		object@layernames <- value
		return(object)
	} else if (inherits(object, "RasterBrick")) {
		object@layernames <- value
		if (length(unique(object@layernames)) != nlayers(object)) {
			stop('layer names must be unique')
		}
		return(object)
	} else if (inherits(object, "RasterStack")) {
		object@layernames <- value
		if (length(unique(object@layernames)) != nlayers(object)) {
			stop('layer names must be unique')
		}
		return(object)
	}	
}
