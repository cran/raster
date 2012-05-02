# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date:  October 2008
# Version 0.9
# Licence GPL v3


.uniqueNames <- function(x, sep='.') {
	y <- as.matrix(table(x))
	y <- y[y[,1] > 1, ,drop=F]
	if (nrow(y) > 0) {
		y <- rownames(y)
		for (i in 1:length(y)) {
			j <- which(x==y[i])
			x[j] <- paste(x[j], sep, 1:length(j), sep='')
		}
	}
	x
}

.goodNames <- function(ln, prefix='layer') {
	ln <- trim(as.character(ln))
	ln[is.na(ln)] <- ""
	if (.standardnames()) {
		ln[ln==''] <- prefix
		ln <- make.names(ln, unique=FALSE)
	}
	.uniqueNames(ln)
}


layerNames <- function(x) {
	ln <- x@layernames
	ln <- ln[1:nlayers(x)]
	.goodNames(as.vector(ln))
}


'layerNames<-' <- function(x, value) {
	nl <- nlayers(x)
	if (is.null(value)) {
		value <- rep('', nl)
	} else if (length(value) != nl) {
		stop('incorrect number of layer names')
	}
	x@layernames <- .goodNames(value)
	if (inherits(x, 'RasterStack')){
		for (i in 1:nl) {
			x@layers[[i]]@layernames <- x@layernames[i]
		}
	}
	return(x)
}

