# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : July 2011
# Version 1.0
# Licence GPL v3



if (!isGeneric("as.data.frame")) {
	setGeneric("as.data.frame", function(x, row.names = NULL, optional = FALSE, ...)
		standardGeneric("as.data.frame"))
}	


.insertColsInDF <- function(x, y, col, combinenames=TRUE) {
	cnames <- NULL
	if (combinenames) {
		if (ncol(y) > 1) {
			cnames <- paste(colnames(x)[col], '_', colnames(y), sep='')
		}
	}
	if (ncol(y) == 1) {
		x[, col] <- y
		return(x)
	} else if (col==1) {
		z <- cbind(y, x[, -1, drop=FALSE])
	} else if (col==ncol(x)) {
		z <- cbind(x[, -ncol(x), drop=FALSE], y)
	} else {
		z <- cbind(x[,1:(col-1), drop=FALSE], y, x[,(col+1):ncol(x), drop=FALSE])
	}
	if (!is.null(cnames)) {
		colnames(z)[col:(col+ncol(y)-1)] <- cnames
	}
	z
}

setMethod('as.data.frame', signature(x='Raster'), 
	function(x, row.names = NULL, optional = FALSE, ...) {

		v <- as.data.frame(values(x), row.names=row.names, optional=optional, ...)
		colnames(v) <- names(x)  # for nlayers = 1
		
		i <- is.factor(x)
		if (any(is.factor(x))) {
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v[,1], 1))
			} else {
				v <- .insertFacts(x, v, 1:nlayers(x))
			}
		}
	
		v
	}
)




