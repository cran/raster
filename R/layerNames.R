# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  October 2008
# Version 0.9
# Licence GPL v3


.enforceGoodLayerNames <- function(x, prefix='layer', returnNames=FALSE) {
	ln <- x@layernames[1:nlayers(x)]
	ln <- trim(ln)
	ln[ln==''] <- 'layer'
	ln <- make.names(ln, unique=TRUE)
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

	nl <- nlayers(object)
	if (is.null(value)) {
		value <- rep('layer', nl)
	} else if (length(value) != nl) {
		stop('incorrect number of layer names')
	}
	value <- trim(as.character(value))
	value[value==''] <- 'layer'
	value <- make.names(value, unique=TRUE)
	
	object@layernames <- value
	return(object)
}
