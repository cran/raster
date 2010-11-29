# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date:  October 2008
# Version 0.9
# Licence GPL v3


.uniqueLayerNames <- function(x) {
	return(length(layerNames(x)) == length(unique(layerNames(x))))
}

.enforceGoodLayerNames <- function(x, prefix='', returnNames=FALSE) {
	if (prefix == '') { prefix <- 'layer'}
	ln <- x@layernames
	ln <- ln[1:nlayers(x)]
	ln[is.na(ln)] <- ""
	
	if (nlayers(x) == 1) {
		if (ln=="") { 
			ln <- prefix
		} 
		if (returnNames) {
			return(ln)
		} else {
			x@layernames <- ln
			return(x)
		}
	}
	
	loops <- seq(along=ln)
	
	for (i in rev(loops)) {
		if (ln[i] %in% ln[-i]) {
			ln[i] <- ''
		}
	}
	
	if (substr(prefix, nchar(prefix), nchar(prefix)) != "_") {
		prefix <- paste(prefix, '_', sep="")
	}
	
	for (i in loops) {
		if (ln[i] == '') {
			cont <- TRUE
			j <- i
			while(cont==TRUE) {
				n <- paste(prefix, j, sep='')
				if (!(n %in% ln)) {
					ln[i] <- n
					cont <- FALSE
				}
				j <- j + i
			}
		}
	}
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
	return(ln)
}


'layerNames<-' <- function(object, value) {
	if (length(value) != nlayers(object)) {
		stop('value has wrong length')
	}
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
